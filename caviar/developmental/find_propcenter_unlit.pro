; image_name='N1866370342_1_cal.IMG'
; @caviar
; .run restore_reproj
; @run_edgefit
; ; At this point, draw a line below the propeller, choose option #8, draw
; ; a line above the propeller, choose option #9, move the bookends, then
; ; choose option #2.  You now have a curve that traces the ridge of
; ; the propeller.  Save this as N1866363047_1_cal.edge1
; ; Note that the variables bke are not included in the saved file
; ; For N1866370342, bke is 782 and 883.
restore, 'N1866370342_1_cal.edge1'
bke1 = 782
redge1 = redge[*,43:83]
bke1 = bke1 + 43
nedge1 = n_elements(redge1[0,*])
restore, 'N1866370342_1_cal.edge2'
redge2 = redge[*, min(where( redge[0,*] ge redge1[0,0] )):$
                  max(where( redge[0,*] le redge1[0,nedge1-1] )) ]
redge2 = redge2[*,vec_remove( indgen(n_elements(redge2[0,*])), [11,15,24] )]
restore, 'N1866370342_1_cal.edge3'
redge3 = redge[*, min(where( redge[0,*] ge redge1[0,0] )):$
                  max(where( redge[0,*] le redge1[0,nedge1-1] )) ]
redge3 = redge3[*,vec_remove( indgen(n_elements(redge3[0,*])), [20,22,26,30] )]

dr = deriv( redge1[0,*], redge1[1,*] )
_ll = redge1[0,14:29]
_dr = dr[14:29]
drfit = gaussfit( _ll, tkm(_dr), aa, nterms=3 )
lcen = aa[1]
rcen = interpol( redge1[1,*], redge1[0,*], lcen )
xcen = interpol( redge1[3,*], redge1[0,*], lcen )
ycen = interpol( dindgen(nedge1) + bke1, redge1[0,*], lcen )

!p.multi = [0,2,2]
!y.omargin = [4,2]
!y.margin = 0
notn = replicate(' ',20)
plot, redge1[0,*], tkm(redge1[1,*]), /xs, /ys, $
      yr=tkm([min(redge2[1,*]),max(redge3[1,*])]), $
      ytit='Radius'+tkmtit(), xtickn=notn;, xtit='Longitude (!Uo!N)'
oplot, redge2[0,*], tkm(redge2[1,*])
oplot, redge3[0,*], tkm(redge3[1,*])
oplot, !x.crange, tkm([rcen,rcen]), l=1
oplot, [lcen,lcen], !y.crange, l=1
!p.multi[0] = !p.multi[0] - 1
plot, redge1[0,*], dr, /xs, /ys, $
      xtit='Longitude (!Uo!N)', ytit='dRadius/dLongitude, (km/!Uo!N)'
oplot, [lcen,lcen], !y.crange, l=1
solid_small_circles
plot, redge1[0,*], dr, /xs, /ys, ps=8, $
      xtit='Longitude (!Uo!N)', ytit='dRadius/dLongitude, (km/!Uo!N)'
oplot, _ll, drfit*1000, l=1

end
