pro update_colour_table

common udct,udctim,udctx,udcty,udctsymbol,udctsymbol_size,udctring_symbol,$
       udctring_symbol_size,udctstars,udctxp,udctyp,udctplanet,udctring,$
       udctxr,udctyr,udcts_names,udctplanet_symbol,udctplanet_symbol_size,$
       udctnl,udctplan_names

tv,udctim,/order

x = round(udctx)
y = round(udcty)
nl = udctnl
stars = udctstars
@plot_stars
xp = round(udctxp)
yp = round(udctyp)
planet = udctplanet
plan_names = udctplan_names
@plot_planets
xr = round(udctxr)
yr = round(udctyr)
ring = udctring
@plot_rings


end
