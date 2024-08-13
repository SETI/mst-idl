blankplot = 1
!p.multi = [0,1,2]
.run sigma_ref
cd, '$DATA/images/077/RDHRCOMP'
restore, 'stretch.sav'
radius077 = reform(_keywords.ringplane_aimpoint_radius)
restore, 'ring_rads_index.sav'
sigma077 = radscan_sigma
solid_circles
oplot, tkm(radius077), sigma077, ps=8

minrad = 74000.0d0
maxrad = 138000.0d0
drad = 100.0d0
radius = dindgen( (maxrad-minrad)/drad + 1 )*drad + minrad
sigma = radius*0

foo = where( radius ge 124700 and radius le 136800 )
foo077 = where( radius077 ge 124700 and radius077 le 136810 )
sigma[foo] = interpol( sigma077[foo077], radius077[foo077], radius[foo] )
foo = where( radius ge 121800 and radius lt 124700 )
foo077 = where( iawave_radi ge 121800 and iawave_radi lt 124700 )
sigma[foo] = interpol( iawave_sigma[foo077], iawave_radi[foo077], radius[foo] )
foo = where( radius ge 116500 and radius le 121800 )
foo077 = where( radius077 ge 116500 and radius077 le 121800 )
sigma[foo] = interpol( sigma077[foo077], radius077[foo077], radius[foo] )
radius_hedman16 = [ 96600.0d0,101500,108700,115560,116600 ]
sigma_hedman16 = [ 47.,40,70,70,54 ]
foo = where( radius ge 96600 and radius lt 116500 )
sigma[foo] = interpol( sigma_hedman16, radius_hedman16, radius[foo] )
foo = where( radius ge 85400 and radius lt 96600 )
foo077 = where( radius077 ge 85400 and radius077 lt 96600 )
sigma[foo] = interpol( sigma077[foo077], radius077[foo077], radius[foo] )
foo = where( radius ge 74600 and radius lt 85400 )
sigma[foo] = 1
oplot, tkm(radius), sigma, co=ctgreen()

plot, tkm(radius077), sigma077, ps=-4, xtit='Radius'+tkmtit(), $
      ytit='Surface Density (g/cm!U2!N)'
oplot, tkm([74490,74490]), !y.crange, l=1
oplot, tkm([91983,91983]), !y.crange, l=1
oplot, tkm([117516,117516]), !y.crange, l=1
oplot, tkm([122053,122053]), !y.crange, l=1
oplot, tkm(radius_hedman16), sigma_hedman16, ps=4, co=ctred()
oplot, tkm(radius), sigma, co=ctgreen()

area = 2*!dpi*radius*drad*1e10   ; in cm^2
annular_mass = sigma*area        ; in g
total_mass = total(annular_mass)
mimas_mass = 37.5e18*1e3         ; in g
