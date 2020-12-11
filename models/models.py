# -*- coding: utf-8 -*-
import math

from odoo import models, fields, api, _
from odoo.exceptions import UserError


class CustomProductPackaging(models.Model):
    _name = 'packaging.info'

    name = fields.Char(required=True)
    qty = fields.Float('Contained Quantity', help="Quantity of products contained in the packaging.")
    capacity = fields.Float(string='Capacity per unit in litres')


class ProductTemplate(models.Model):
    _inherit = 'product.template'
    packaging_info = fields.Many2one('packaging.info')


class LiquidTaxSystem(models.Model):
    _inherit = 'account.tax'

    include_discount = fields.Boolean(compute='base_includes_discount')
    is_consumption = fields.Boolean(compute='is_consumption')
    is_custom_tax = fields.Boolean(compute='is_custom_tax')

    @api.depends('amount_type')
    def is_custom_tax(self):
        for tax in self:
            tax.is_custom_tax = tax.amount_type not in ('group', 'percent', 'division', 'fixed')

    @api.depends('amount_type')
    def is_consumption(self):
        for tax in self:
            tax.is_consumption = tax.amount_type[:4] == 'cons'

    @api.depends('amount_type')
    def base_includes_discount(self):
        for tax in self:
            tax.include_discount = tax.amount_type[:4] in ('cons', 'pfl')

    amount_type = fields.Selection(default='percent', string="Tax Computation", required=True,
                                   selection=[
                                       ('group', 'Group of Taxes'), ('fixed', 'Fixed'),
                                       ('percent', 'Percentage of Price'),
                                       ('division', 'Percentage of Price Tax Included'),
                                       ('cons_percent', 'CONS_%'), ('cons_300', 'CONS_300'),
                                       ('cons_125', 'CONS_125'), ('cons_0', 'CONS_0'),
                                       ('tva', 'TVA_%'), ('pfl', 'PFL_%')
                                   ],
                                   help="""
       - Group of Taxes: The tax is a set of sub taxes.
       - Fixed: The tax amount stays the same whatever the price.
       - Percentage of Price: The tax amount is a % of the price:
           e.g 100 * 10% = 110 (not price included)
           e.g 110 / (1 + 10%) = 100 (price included)
       - Percentage of Price Tax Included: The tax amount is a division of the price:
           e.g 180 / (1 - 10%) = 200 (not price included)
           e.g 200 * (1 - 10%) = 180 (price included)
           """)

    def _compute_amount(self, base_amount, price_unit, quantity=1.0, product=None, partner=None,
                        packaging=None):
        """ Returns the amount of a single tax. base_amount is the actual amount on which the tax is applied, which is
            price_unit * quantity eventually affected by previous taxes (if tax is include_base_amount XOR price_include)
        """
        if self.amount_type == 'fixed':
            # Use copysign to take into account the sign of the base amount which includes the sign
            # of the quantity and the sign of the price_unit
            # Amount is the fixed price for the tax, it can be negative
            # Base amount included the sign of the quantity and the sign of the unit price and when
            # a product is returned, it can be done either by changing the sign of quantity or by changing the
            # sign of the price unit.
            # When the price unit is equal to 0, the sign of the quantity is absorbed in base_amount then
            # a "else" case is needed.
            if base_amount:
                return math.copysign(quantity, base_amount) * self.amount
            else:
                return quantity * self.amount

        price_include = self._context.get('force_price_include', self.price_include)

        # base * (1 + tax_amount) = new_base
        if self.amount_type == 'percent' and not price_include:
            return base_amount * self.amount / 100
        # <=> new_base = base / (1 + tax_amount)
        if self.amount_type == 'percent' and price_include:
            return base_amount - (base_amount / (1 + self.amount / 100))
        # base / (1 - tax_amount) = new_base
        if self.amount_type == 'division' and not price_include:
            return base_amount / (1 - self.amount / 100) - base_amount if (1 - self.amount / 100) else 0.0
        # <=> new_base * (1 - tax_amount) = base
        if self.amount_type == 'division' and price_include:
            return base_amount - (base_amount * (self.amount / 100))

        if self.amount_type == 'cons_0':
            return 0.0
        if self.amount_type == 'cons_percent' and not price_include:
            return base_amount * self.amount / 100
        if self.amount_type == 'cons_percent' and price_include:
            return base_amount - (base_amount / (1 + self.amount / 100))
        if self.amount_type in ('cons_125', 'cons_300') and not packaging:
            raise UserError(_(f"Tax {self.name} applies to packaged products only"))
        if self.amount_type == 'cons_300':
            return quantity * packaging.qty * packaging.capacity * 300
        if self.amount_type == 'cons_125':
            return quantity * packaging.qty * packaging.capacity * 125
        if self.amount_type == 'tva' and not price_include:
            return base_amount * self.amount / 100
        if self.amount_type == 'tva' and price_include:
            return base_amount - (base_amount / (1 + self.amount / 100))
        if self.amount_type == 'pfl' and not price_include:
            return base_amount * self.amount / 100
        if self.amount_type == 'pfl' and price_include:
            return base_amount - (base_amount / (1 + self.amount / 100))

    def compute_all(self, price_unit, currency=None, quantity=1.0, product=None, partner=None, is_refund=False,
                    handle_price_include=True, packaging=None):
        """ Returns all information required to apply taxes (in self + their children in case of a tax group).
            We consider the sequence of the parent for group of taxes.
                Eg. considering letters as taxes and alphabetic order as sequence :
                [G, B([A, D, F]), E, C] will be computed as [A, D, F, C, E, G]

            'handle_price_include' is used when we need to ignore all tax included in price. If False, it means the
            amount passed to this method will be considered as the base of all computations.

        RETURN: {
            'total_excluded': 0.0,    # Total without taxes
            'total_included': 0.0,    # Total with taxes
            'total_void'    : 0.0,    # Total with those taxes, that don't have an account set
            'taxes': [{               # One dict for each tax in self and their children
                'id': int,
                'name': str,
                'amount': float,
                'sequence': int,
                'account_id': int,
                'refund_account_id': int,
                'analytic': boolean,
            }],
        } """
        if not self:
            company = self.env.company
        else:
            company = self[0].company_id

        # 1) Flatten the taxes.
        taxes = self.flatten_taxes_hierarchy()

        # 2) Avoid mixing taxes having price_include=False && include_base_amount=True
        # with taxes having price_include=True. This use case is not supported as the
        # computation of the total_excluded would be impossible.
        base_excluded_flag = False  # price_include=False && include_base_amount=True
        included_flag = False  # price_include=True
        for tax in taxes:
            if tax.price_include:
                included_flag = True
            elif tax.include_base_amount:
                base_excluded_flag = True
            if base_excluded_flag and included_flag:
                raise UserError(_(
                    'Unable to mix any taxes being price included with taxes affecting the base amount but not included in price.'))

        # 3) Deal with the rounding methods
        if not currency:
            currency = company.currency_id
        # By default, for each tax, tax amount will first be computed
        # and rounded at the 'Account' decimal precision for each
        # PO/SO/invoice line and then these rounded amounts will be
        # summed, leading to the total amount for that tax. But, if the
        # company has tax_calculation_rounding_method = round_globally,
        # we still follow the same method, but we use a much larger
        # precision when we round the tax amount for each line (we use
        # the 'Account' decimal precision + 5), and that way it's like
        # rounding after the sum of the tax amounts of each line
        prec = currency.decimal_places

        # In some cases, it is necessary to force/prevent the rounding of the tax and the total
        # amounts. For example, in SO/PO line, we don't want to round the price unit at the
        # precision of the currency.
        # The context key 'round' allows to force the standard behavior.
        round_tax = False if company.tax_calculation_rounding_method == 'round_globally' else True
        round_total = True
        if 'round' in self.env.context:
            round_tax = bool(self.env.context['round'])
            round_total = bool(self.env.context['round'])

        if not round_tax:
            prec += 5

        # 4) Iterate the taxes in the reversed sequence order to retrieve the initial base of the computation.
        #     tax  |  base  |  amount  |
        # /\ ----------------------------
        # || tax_1 |  XXXX  |          | <- we are looking for that, it's the total_excluded
        # || tax_2 |   ..   |          |
        # || tax_3 |   ..   |          |
        # ||  ...  |   ..   |    ..    |
        #    ----------------------------
        def recompute_base(base_amount, fixed_amount, percent_amount, division_amount, prec):
            # Recompute the new base amount based on included fixed/percent amounts and the current base amount.
            # Example:
            #  tax  |  amount  |   type   |  price_include  |
            # -----------------------------------------------
            # tax_1 |   10%    | percent  |  t
            # tax_2 |   15     |   fix    |  t
            # tax_3 |   20%    | percent  |  t
            # tax_4 |   10%    | division |  t
            # -----------------------------------------------

            # if base_amount = 145, the new base is computed as:
            # (145 - 15) / (1.0 + 30%) * 90% = 130 / 1.3 * 90% = 90
            return round((base_amount - fixed_amount) / (1.0 + percent_amount / 100.0) * (100 - division_amount) / 100,
                         prec)

        base = round(price_unit * quantity, prec)

        # For the computation of move lines, we could have a negative base value.
        # In this case, compute all with positive values and negate them at the end.
        sign = 1
        if base <= 0:
            base = -base
            sign = -1

        # Store the totals to reach when using price_include taxes (only the last price included in row)
        total_included_checkpoints = {}
        i = len(taxes) - 1
        store_included_tax_total = True
        # Keep track of the accumulated included fixed/percent amount.
        incl_fixed_amount = incl_percent_amount = incl_division_amount = 0
        # Store the tax amounts we compute while searching for the total_excluded
        cached_tax_amounts = {}
        if handle_price_include:
            for tax in reversed(taxes):
                tax_repartition_lines = (
                        is_refund
                        and tax.refund_repartition_line_ids
                        or tax.invoice_repartition_line_ids
                ).filtered(lambda x: x.repartition_type == "tax")
                sum_repartition_factor = sum(tax_repartition_lines.mapped("factor"))
                if tax.include_base_amount and not tax.is_custom_tax:
                    base = recompute_base(base, incl_fixed_amount, incl_percent_amount, incl_division_amount, prec)
                    incl_fixed_amount = incl_percent_amount = incl_division_amount = 0
                    store_included_tax_total = True
                if tax.price_include or self._context.get('force_price_include'):
                    if tax.amount_type == 'percent':
                        incl_percent_amount += tax.amount * sum_repartition_factor
                    elif tax.amount_type == 'division':
                        incl_division_amount += tax.amount * sum_repartition_factor
                    elif tax.amount_type == 'fixed':
                        incl_fixed_amount += quantity * tax.amount * sum_repartition_factor
                    else:
                        # tax.amount_type == other (python)
                        tax_amount = tax._compute_amount(base, sign * price_unit, quantity, product,
                                                         partner, packaging) * sum_repartition_factor
                        incl_fixed_amount += tax_amount
                        # Avoid unecessary re-computation
                        cached_tax_amounts[i] = tax_amount
                    # In case of a zero tax, do not store the base amount since the tax amount will
                    # be zero anyway. Group and Python taxes have an amount of zero, so do not take
                    # them into account.
                    if store_included_tax_total and (
                            tax.amount or tax.amount_type not in ("percent", "division", "fixed")
                    ):
                        total_included_checkpoints[i] = base
                        store_included_tax_total = False
                i -= 1

        total_excluded = recompute_base(base, incl_fixed_amount, incl_percent_amount, incl_division_amount, prec)

        # 5) Iterate the taxes in the sequence order to compute missing tax amounts.
        # Start the computation of accumulated amounts at the total_excluded value.
        base = total_included = total_void = total_excluded

        taxes_vals = []
        i = 0
        cumulated_tax_included_amount = 0
        for tax in taxes:
            tax_repartition_lines = (
                    is_refund and tax.refund_repartition_line_ids or tax.invoice_repartition_line_ids).filtered(
                lambda x: x.repartition_type == 'tax')
            sum_repartition_factor = sum(tax_repartition_lines.mapped('factor'))

            price_include = self._context.get('force_price_include', tax.price_include)

            # compute the tax_amount
            if price_include and total_included_checkpoints.get(i):
                # We know the total to reach for that tax, so we make a substraction to avoid any rounding issues
                tax_amount = total_included_checkpoints[i] - (base + cumulated_tax_included_amount)
                cumulated_tax_included_amount = 0
            else:
                tax_amount = tax.with_context(force_price_include=False)._compute_amount(
                    base, sign * price_unit, quantity, product, partner, packaging)

            # Round the tax_amount multiplied by the computed repartition lines factor.
            tax_amount = round(tax_amount, prec)
            factorized_tax_amount = round(tax_amount * sum_repartition_factor, prec)

            if price_include and not total_included_checkpoints.get(i):
                cumulated_tax_included_amount += factorized_tax_amount

            # If the tax affects the base of subsequent taxes, its tax move lines must
            # receive the base tags and tag_ids of these taxes, so that the tax report computes
            # the right total
            subsequent_taxes = self.env['account.tax']
            subsequent_tags = self.env['account.account.tag']
            if tax.include_base_amount and not tax.is_custom_tax:
                subsequent_taxes = taxes[i + 1:]
                subsequent_tags = subsequent_taxes.get_tax_tags(is_refund, 'base')

            # Compute the tax line amounts by multiplying each factor with the tax amount.
            # Then, spread the tax rounding to ensure the consistency of each line independently with the factorized
            # amount. E.g:
            #
            # Suppose a tax having 4 x 50% repartition line applied on a tax amount of 0.03 with 2 decimal places.
            # The factorized_tax_amount will be 0.06 (200% x 0.03). However, each line taken independently will compute
            # 50% * 0.03 = 0.01 with rounding. It means there is 0.06 - 0.04 = 0.02 as total_rounding_error to dispatch
            # in lines as 2 x 0.01.
            repartition_line_amounts = [round(tax_amount * line.factor, prec) for line in tax_repartition_lines]
            total_rounding_error = round(factorized_tax_amount - sum(repartition_line_amounts), prec)
            nber_rounding_steps = int(abs(total_rounding_error / currency.rounding))
            rounding_error = round(nber_rounding_steps and total_rounding_error / nber_rounding_steps or 0.0, prec)

            for repartition_line, line_amount in zip(tax_repartition_lines, repartition_line_amounts):

                if nber_rounding_steps:
                    line_amount += rounding_error
                    nber_rounding_steps -= 1

                taxes_vals.append({
                    'id': tax.id,
                    'name': partner and tax.with_context(lang=partner.lang).name or tax.name,
                    'amount': sign * line_amount,
                    'base': round(sign * base, prec),
                    'sequence': tax.sequence,
                    'account_id': tax.cash_basis_transition_account_id.id if tax.tax_exigibility == 'on_payment' else repartition_line.account_id.id,
                    'analytic': tax.analytic,
                    'price_include': price_include,
                    'tax_exigibility': tax.tax_exigibility,
                    'tax_repartition_line_id': repartition_line.id,
                    'tag_ids': (repartition_line.tag_ids + subsequent_tags).ids,
                    'tax_ids': subsequent_taxes.ids,
                    'amount_type': tax.amount_type,
                })

                if not repartition_line.account_id:
                    total_void += line_amount

            # Affect subsequent taxes
            if tax.include_base_amount and not tax.is_custom_tax:
                base += factorized_tax_amount

            total_included += factorized_tax_amount
            i += 1

        return {
            'base_tags': taxes.mapped(
                is_refund and 'refund_repartition_line_ids' or 'invoice_repartition_line_ids').filtered(
                lambda x: x.repartition_type == 'base').mapped('tag_ids').ids,
            'taxes': taxes_vals,
            'total_excluded': sign * (currency.round(total_excluded) if round_total else total_excluded),
            'total_included': sign * (currency.round(total_included) if round_total else total_included),
            'total_void': sign * (currency.round(total_void) if round_total else total_void),
        }

    def advanced_compute_all(self, price_unit, currency=None, quantity=1.0, product=None, partner=None,
                             is_refund=False, discount=0.0, packaging=None, handle_price_include=False):

        taxes = self.flatten_taxes_hierarchy()
        consumption_tax = 0.0
        for tax in taxes:
            if tax.amount_type[:4] == 'cons':
                vals = tax.compute_all(price_unit, currency, quantity, product=product, partner=partner,
                                       packaging=packaging, is_refund=is_refund,
                                       handle_price_include=handle_price_include)['taxes']
                for tax_obj in vals:
                    consumption_tax += tax_obj['amount']

        taxes_res = {'base_tags': [], 'taxes': [], 'total_excluded': -0.0, 'total_included': -0.0, 'total_void': -0.0}
        price_without_vat = price_unit + (consumption_tax / quantity)
        net_commercial = price_without_vat * (1 - (discount or 0.0) / 100.0)
        for tax in taxes:
            if tax.include_discount:
                values = tax.compute_all(price_unit, currency, quantity, product=product, partner=partner,
                                         packaging=packaging, is_refund=is_refund,
                                         handle_price_include=handle_price_include)
            else:
                values = tax.compute_all(net_commercial, currency, quantity, product=product, partner=partner,
                                         packaging=packaging,
                                         is_refund=is_refund, handle_price_include=handle_price_include)
            if tax.amount_type[:4] == 'cons':
                for tax_obj in values['taxes']:
                    consumption_tax += tax_obj['amount']
            taxes_res = {k: v + values[k] for k, v in taxes_res.items()}
        clean_taxes = [tax for tax in taxes_res['taxes'] if tax['amount_type'][:4] != 'cons']
        taxes_res['taxes'] = clean_taxes
        discount = price_without_vat * quantity * (discount or 0.0) / 100.0
        prec = currency.decimal_places
        taxes_res['total_excluded'] = round((price_without_vat * quantity) - discount, prec)
        taxes_res['total_included'] = taxes_res['total_excluded'] + sum([tax['amount'] for tax in taxes_res['taxes']])

        return taxes_res


class SaleOrderLine(models.Model):
    _inherit = 'sale.order.line'

    @api.depends('product_uom_qty', 'discount', 'price_unit', 'tax_id', 'order_id')
    def _compute_amount(self):
        """
        Compute the amounts of the SO line.
        """
        for line in self:
            packaging = line.product_id.packaging_info
            taxes = line.tax_id.advanced_compute_all(line.price_unit, currency=line.order_id.currency_id,
                                                     quantity=line.product_uom_qty,
                                                     product=line.product_id, partner=line.order_id.partner_shipping_id,
                                                     packaging=packaging,
                                                     discount=line.discount)
            print("taxes", taxes)
            line.update({
                'price_tax': sum(t.get('amount', 0.0) for t in taxes.get('taxes', [])),
                'price_total': taxes['total_included'],
                'price_subtotal': taxes['total_excluded'],
            })
            if self.env.context.get('import_file', False) and not self.env.user.user_has_groups(
                    'account.group_account_manager'):
                line.tax_id.invalidate_cache(['invoice_repartition_line_ids'], [line.tax_id.id])


class AccountMove(models.Model):
    _inherit = 'account.move'

    def _recompute_tax_lines(self, recompute_tax_base_amount=False):
        pass


class AccountMoveLine(models.Model):
    _inherit = 'account.move.line'

    @api.model
    def _get_price_total_and_subtotal_model(self, price_unit, quantity, discount, currency, product, partner, taxes,
                                            move_type):
        pass
