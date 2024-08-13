; Creates Figure S6 from Jones et al (2008), using Equation S10
plot, [20,200], [0,1], /xs, /ys, /xlog, /nodata, $
      xtit='Energy (keV)', ytit='Absorption probability'
ee = 20 * 10^(findgen(100)/99*alog10(15))
rg = findgen(100)/99*9 + 3
utb = 780*2 - findgen(100)/99*620*2
dd = 210.0d0
oplot, ee, 2*dd/utb

end
