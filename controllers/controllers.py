# -*- coding: utf-8 -*-
# from odoo import http


# class LiquidTaxSystem(http.Controller):
#     @http.route('/liquid_tax_system/liquid_tax_system/', auth='public')
#     def index(self, **kw):
#         return "Hello, world"

#     @http.route('/liquid_tax_system/liquid_tax_system/objects/', auth='public')
#     def list(self, **kw):
#         return http.request.render('liquid_tax_system.listing', {
#             'root': '/liquid_tax_system/liquid_tax_system',
#             'objects': http.request.env['liquid_tax_system.liquid_tax_system'].search([]),
#         })

#     @http.route('/liquid_tax_system/liquid_tax_system/objects/<model("liquid_tax_system.liquid_tax_system"):obj>/', auth='public')
#     def object(self, obj, **kw):
#         return http.request.render('liquid_tax_system.object', {
#             'object': obj
#         })
