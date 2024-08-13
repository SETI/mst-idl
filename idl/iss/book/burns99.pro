if keyword_set(dolzr) then begin
  lzr, 'burns99plot'
  @plot_prepare
  plot_color
endif

!p.multi = [0,2,2]
!p.charsize = 1.5
!y.margin = 0
!y.omargin = [4,2]
nv = 1000
v = findgen(nv)/100
plot, [0.5,nv/100], [.003,1.9], /nodata, /xs, /ys, /xlog, /ylog, $
      ytit='Amount of Ejecta', $
      xtit='Source Moon Radius R/R!Dcrit!N'
oplot, v[100:nv-1], v[100:nv-1]^(-1./4), thick=10
oplot, v[0:100], v[0:100]^2, thick=10
oplot, v[100:nv-1], v[100:nv-1]^(-9./4), co=ctred();, l=1
oplot, v[0:100], v[0:100]^0, co=ctred();, l=1
oplot, v, v^2/100, co=ctblue()
; (969,1818) to (2300,652)
xyouts, .654, .0075, 'Total produced!C(!MFYR!U2!N, arbitrary units)', $
        orient=atan((1818.-652)/(2300-969))*180/!dpi, chars=1, co=ctblue()
; (1245,654) to (2300,1690)
xyouts, 4, .08, 'Fraction escaping!Cmoon''s gravity (F!De!N)', $
        orient=atan((654.-1690)/(2300-1245))*180/!dpi, chars=1, co=ctred()
; (1245,654) to (2300,768)
xyouts, 1.3, 1.45, 'Total contribution to dusty ring!C(dM/dt, arbitrary units)',$
        orient=atan((654.-768)/(2300-1245))*180/!dpi, chars=1
if keyword_set(dolzr) then clzr

end
