if keyword_set(dolzr) then begin
  if keyword_set(bb) then begin
    lzr, 'sigma_ref_ringsres3b'
    @plot_prepare
    !p.multi = [0,1,2]
  endif else if keyword_set(vcass) then begin
    lzr, 'sigma_ref_ringsres3vcass'
    @plot_prepare
    !p.multi = [0,2,2]
  endif else begin
    lzr, 'sigma_ref_ringsres3'
    @plot_prepare
    !p.multi = [0,2,2]
  endelse
endif
restore, '$DATA/images/116/EQXSHADOW001/iawave_sigmafit.sav'
iawave_radi = radi
iawave_sigma = sigmafit
if keyword_set(bb) then ymax=75 else ymax=45
if keyword_set(vcass) then ymax=65
if keyword_set(bb) then xmin=90 else xmin=117.6
plot, [xmin,138], [0,ymax], /nodata, /xs, /ys, xtit='Radius'+tkmtit(), $
      ytit='Surface Density (g/cm!U2!N)'
oplot, tkm(iawave_radi), iawave_sigma
oplot, [122.05,122.05], !y.crange, l=1
oplot, [117.58,117.58], !y.crange, l=1
oplot, [92,92], !y.crange, l=1
restore, '$DATA/images/077/RDHRCOMP/stretch.sav'
radius077 = reform(_keywords.ringplane_aimpoint_radius)
restore, '$DATA/images/077/RDHRCOMP/ring_rads_index.sav'
sigma077 = radscan_sigma
solid_circles
radius077 = radius077[0:28]
sigma077 = sigma077[0:28]
radius046 = [ 122100.0d0, 121260, 120960, 120670, 118820, 118450 ]
sigma046 = [ 18.0d0, 11.5, 8, 3.4, 1.5, 1 ]
radius071 = [ 95200d0, 95500, 96000, 100500 ]
sigma071 = [ 25.0d0, 25, 25, 43 ]
radiushed = [ 96600.0d0, 101500, 108700, 115500, 116100 ]
sigmahed = [ 46.0d0, 40, 67, 67, 130 ]
radiusliss = 116600.0d0
sigmaliss = 54.0d0
;radius077 = vec_remove( radius077, 10 )
;sigma077 = vec_remove( sigma077, 10 )
oplot, tkm(radius077[0:9]), sigma077[0:9], ps=-8, l=1
oplot, tkm(radius077[11:28]), sigma077[11:28], ps=-8, l=1
oplot, tkm(radius046), sigma046, ps=8
oplot, tkm(radius071), sigma071, ps=8
open_squares
oplot, tkm(radiushed), sigmahed, ps=8
foo = where( sigmahed gt ymax, count )
if keyword_set(bb) then for j=0,count-1 do begin
  arrow, tkm(radiushed[foo[j]]), $
         !y.crange[1] - (!y.crange[1]-!y.crange[0])*.15, $
         tkm(radiushed[foo[j]]), $
         !y.crange[1] - (!y.crange[1]-!y.crange[0])*.05, $
         hthick=1, /solid, hsize=!d.x_size/128, /data
endfor
open_circles
oplot, [tkm(radiusliss)], [sigmaliss], ps=8
oplot, [133.425d0,133.425], !y.crange, l=1
oplot, [133.745d0,133.745], !y.crange, l=1
oplot, [136.505d0,136.505], !y.crange, l=1
oplot, [136.8d0,136.8], !y.crange, l=1
xyouts, 121.9, 1, align=1, 'CD'
if keyword_set(bb) then ax = mean([122.05,136.8]) else ax = 122.3
if keyword_set(bb) then aligna = 0.5 else aligna = 0
xyouts, ax, 1, 'A', align=aligna
xyouts, mean([92,117.58]), 1, 'B', align=.5
xyouts, 91.6, 1, 'C', align=1
xyouts, 133.72, 1, 'Encke', orient=90
xyouts, 136.48, 1, 'Keeler', orient=90
restore, '$DATA/caviar/developmental/sigma_ref.sav'
if keyword_set(vcass) then begin
  open_circles
  oplot, tkm(cassini_rres[where(nonlinear)]), $
         cassini_sigma[where(nonlinear)], ps=8
  open_circles
  oplot, tkm(cassini_rres[where(nonlinear eq 0)]), $
         cassini_sigma[where(nonlinear eq 0)], ps=8
  open_circles, radius=0.4
  oplot, tkm(voyager_rres), voyager_sigma, ps=8
endif

if keyword_set(dolzr) then clzr

end
