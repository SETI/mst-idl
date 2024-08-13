; image_name='N1866363047_1_cal.IMG'
; @caviar
; .run restore_reproj
; @run_edgefit
; ; At this point, draw a line below the propeller, choose option #8, draw
; ; a line above the propeller, choose option #9, move the bookends, then
; ; choose option #1.  You now have a curve that traces the ridge of
; ; the propeller.  Save this as N1866363047_1_cal.edge1
; ; Note that the variables bke are not included in the saved file
; ; For N1866363047, bke is 426 and 602.
restore, 'N1866363047_1_cal.edge1'
bke1 = 426
nedge = n_elements(redge[0,*])
dy = deriv(redge[3,*])
dr = deriv( redge[0,*], redge[1,*] )
xx = dindgen(nedge) + bke1
dyfit = gaussfit( redge[0,*], dy, aa, nterms=3 )
lcen = aa[1]
rcen = interpol( redge[1,*], redge[0,*], lcen )
xcen = interpol( redge[3,*], redge[0,*], lcen )
ycen = interpol( xx, redge[0,*], lcen )

!p.multi = [0,3,2]
!p.charsize = 2
!y.omargin = [4,2]
!y.margin = 0
notn = replicate(' ',20)
plot, redge[0,*], tkm(redge[1,*]), /xs, /ys, $
      ytit='Radius'+tkmtit(), xtickn=notn;, xtit='Longitude (!Uo!N)'
oplot, !x.crange, tkm([rcen,rcen]), l=1
oplot, [lcen,lcen], !y.crange, l=1
plot, xx, redge[3,*], /xs, /ys, $
      ytit='Reprojected Y', xtickn=notn;, xtit='Reprojected X'
oplot, !x.crange, [xcen,xcen], l=1
oplot, [ycen,ycen], !y.crange, l=1
!p.multi[0] = !p.multi[0] - 1
plot, redge[0,*], dr, /xs, /ys, $
      xtit='Longitude (!Uo!N)', ytit='dRadius/dLongitude, (km/!Uo!N)'
oplot, [lcen,lcen], !y.crange, l=1
plot, xx, dy, /xs, /ys, $
      xtit='Reprojected X', ytit='Reprojected dY/dX'
oplot, [ycen,ycen], !y.crange, l=1
solid_small_circles
plot, xx, dy, /xs, /ys, ps=8, $
      xtit='Reprojected X', ytit='Reprojected dY/dX'
oplot, xx, dyfit, l=1

end
