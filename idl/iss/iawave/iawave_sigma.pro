sigma = dblarr(n_elements(radi))
j75 = (ring_rads[where( ring_rads_legend eq 'Ja 7:5' )])[0]
a65 = resloc(6,5,615)
p54 = resloc(5,4,617)
a76 = resloc(7,6,615)
pan109 = resloc(10,9,618)
aredge = (ring_rads[where( ring_rads_legend eq 'IER 7 A Ring' )])[0]
a1 = 122.3d3
a2 = 122.4e3
a3 = 123.4e3
a4 = 123.8d3
a5 = 125.2d3
sigma2 = 27
sigma3 = 30
sigma4 = 33
sigma5 = 35
cassdiv = where( radi lt a1 )
aring1 = where( radi ge a1 and radi lt a2 )
aring2 = where( radi ge a2 and radi lt a3 )
aring3 = where( radi ge a3 and radi lt a4 )
aring4 = where( radi ge a4 )

if not keyword_set(slopecd) then slopecd = 0.0075
sigma[cassdiv] = slopecd*(radi[cassdiv]-j75) + 11.5
slopea1 = (sigma2-sigma[cassdiv[n_elements(cassdiv)-1]])/(a2-a1)
sigma[aring1] = slopea1*(radi[aring1]-a1) + sigma[cassdiv[n_elements(cassdiv)-1]]
slopea2 = (sigma3-sigma2)/(a3-a2)
sigma[aring2] = slopea2*(radi[aring2]-a2) + sigma2
slopea3 = (sigma4-sigma3)/(a4-a3)
sigma[aring3] = slopea3*(radi[aring3]-a3) + sigma3
slopea4 = (sigma5-sigma4)/(a5-a4)
sigma[aring4] = slopea4*(radi[aring4]-a4) + sigma4

plot, tkm(radi), sigma, xr=wavelet_xr, xtit='Radius'+tkmtit(), $
      ytit='Model surface density (g/cm!U2!N)', /xs
solid_squares
oplot, [tkm(j75),123], [11.5,10], ps=8
xyouts, 123.15, 9.2, 'Tiscareno et al (2006, ApJL)', chars=1
solid_circles
oplot, [tkm(a76),123], [32.6,7], ps=8
;oplot, [tkm(pan109)], [37.7], ps=8
xyouts, 123.15, 6.2, 'Tiscareno et al (2007, Icarus)', chars=1
solid_triangles
oplot, [tkm(a65),123], [15.4,4], ps=8
xyouts, 123.15, 3.2, 'Colwell et al (2009, Icarus)', chars=1
solid_stars
oplot, [tkm(p54)], [28], ps=8
oplot, [1,1]*tkm(aredge), !y.crange, l=1
xyouts, tkm(j75)+.1, 11.5-1.5, chars=1, 'J/E 7:5'
xyouts, tkm(a65)+.1, 15.4-1, chars=1, 'Atlas 6:5'
xyouts, tkm(p54), 28+1, chars=1, /align, 'Pandora 5:4'
xyouts, tkm(a76), 32.6-2.5, chars=1, 'Atlas 7:6'
;xyouts, tkm(pan109), 37.7+1, chars=1, /align, 'Pan 10:9'
oplot, [121.1,121.4], [37,37]
xyouts, 121.5, 36, 'Iapetus -1:0 wave train model', chars=1.5
