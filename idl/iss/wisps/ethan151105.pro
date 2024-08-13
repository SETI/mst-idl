; Edited inputs from Ethan's emails of 2015-10-26 (5:25pm),
; 2015-10-30 (11:26am), and 2015-11-01 (10:03am)
; 
; 1A:
; [  -0.69163146  443.06148705]
; [ 0.0034304   1.65055255]
; 
; 1B:
; [  -0.71429462  456.06467081]
; [ 0.00232653  1.12092187]
; 
; 2:
; [  -0.60302361  409.93013562]
; [  0.04097754  19.70593208]
; 
; 3:
; [  -0.62579808  425.2474013 ]
; [  0.02661224  12.82062396]
; 
; 4:
; [  -0.71702138  472.74575152]
; [ 0.00813463  3.91401413]
; 
; 5:
; [  -0.70869189  474.76745746]
; [ 0.00337995  1.63103097]
; 
; 6:
; [  -0.70869189  474.76745746]
; [ 0.00337995  1.63103097]
; 
; 7:
; [ -0.72350968  499.04151389]
; [ 0.0008976  0.43250956]
; 
; 8:
; [  -0.72442844  508.76111649]
; [ 0.00353013  1.70500728]
; 
; 8B:
; [  -0.72489302  514.06793104]
; [ 0.00483119  2.33340055]
; 
; 9A:
; [  -0.71554429  513.26174739]
; [  0.03890626  18.71870469]
; 
; 9:
; [  -0.667943    492.80407267]
; [  0.02746013  13.21256749]
; 
; 10:
; [  -0.71732816  520.70378697]
; [  0.04041052  19.46804175]
; 
; 11:
; [  -0.71884176  528.28724834]
; [ 0.00544683  2.62076565]
; 
; 12:
; [  -0.73213573  537.72984979]
; [  0.14689891  70.76954   ]
; 
; 13:
; [  -0.8169186   590.13278765]
; [  0.04154049  20.0640752 ]

wispname = [ '1A', '1B', '2', '3', '4', '5', '6', '7', '8', '8B', '9A', '9', $
             '10', '11', '12', '13' ]

wisps = [ [ -0.69163146, 443.06148705, 0.0034304, 1.65055255 ], $
          [ -0.71429462, 456.06467081, 0.00232653, 1.12092187 ], $
          [ -0.60302361, 409.93013562, 0.04097754, 19.70593208 ], $
          [ -0.62579808, 425.2474013, 0.02661224, 12.82062396 ], $
          [ -0.71702138, 472.74575152, 0.00813463, 3.91401413 ], $
          [ -0.70869189, 474.76745746, 0.00337995, 1.63103097 ], $
          [ -0.70869189, 474.76745746, 0.00337995, 1.63103097 ], $
          [ -0.72350968, 499.04151389, 0.0008976, 0.43250956 ], $
          [ -0.72442844, 508.76111649, 0.00353013, 1.70500728 ], $
          [ -0.72489302, 514.06793104, 0.00483119, 2.33340055 ], $
          [ -0.71554429, 513.26174739, 0.03890626, 18.71870469 ], $
          [ -0.667943, 492.80407267, 0.02746013, 13.21256749 ], $
          [ -0.71732816, 520.70378697, 0.04041052, 19.46804175 ], $
          [ -0.71884176, 528.28724834, 0.00544683, 2.62076565 ], $
          [ -0.73213573, 537.72984979, 0.14689891, 70.76954 ], $
          [ -0.8169186, 590.13278765, 0.04154049, 20.0640752 ] ]

daphnis = 136504.2d0
kgoe = 136522.4d0

solid_circles
notn = replicate(' ',20)
nedge = sqrt(caviar_omega2(kgoe)) - sqrt(caviar_omega2(daphnis))
nedge = nedge*180/!dpi*86400*7
if keyword_set(dolzr) then begin
  lzr, 'ethan151105i', /half
  @plot_prepare
  plot_color
endif

!p.multi = [0,1,2]
device, decomposed=0
for k=0,1 do begin
  if k eq 0 then foo = where( wisps[2,*] eq wisps[2,*], count ) else begin
    foo = where( wisps[2,*] lt .02, count )
  endelse
  fit = svdfit( indgen(count), reform(wisps[0,foo]), 1, $
                measure_errors=reform(wisps[2,foo]), $
                sigma=fit_sigma, chisq=fit_chisq )
  print, strtrim(fit,2)+' +- '+strtrim(fit_sigma,2)
  print, strtrim(caviar_omega_to_r(fit/180*!dpi/86400/7+$
                                   sqrt(caviar_omega2(136504.2d0))),2)+' +- '+$
         strtrim(caviar_omega_to_r((fit-fit_sigma)/180*!dpi/86400/7+$
                                   sqrt(caviar_omega2(136504.2d0))) - $
                 caviar_omega_to_r(fit/180*!dpi/86400/7+$
                                   sqrt(caviar_omega2(136504.2d0))),2)
  plot, [-1,16], [0,0], $
        yr=[ max(wisps[0,foo]+wisps[2,foo]) + .02/(k+1), $
             min(wisps[0,foo]-wisps[2,foo]) - .02/(k+1) ], $
        xs=1, xtickn=notn, ys=5, /nodata, xtickle=1e-10, xma=[10,10]
  polyfill, !x.crange[[0,1,1,0,0]], [nedge,nedge,-1,-1,nedge], $
            co=ltgray(), noclip=0
  oploterr, foo, wisps[0,foo], wisps[2,foo], 8
  oplot, !x.crange, [fit,fit]
  oplot, !x.crange, [fit,fit] + [fit_sigma,fit_sigma], l=1
  oplot, !x.crange, [fit,fit] - [fit_sigma,fit_sigma], l=1
  axis, yaxis=0, ytit='Daphnis-Relative!CMean Motion (deg/week)', $
        yr=!y.crange, /ys
  axis, yaxis=1, /ys, $
        yr=caviar_omega_to_r(!y.crange/180*!dpi/86400/7 + $
                             sqrt(caviar_omega2(daphnis))) - daphnis, $
        ytit='Daphnis-Relative!CSemimajor Axis (km)'
endfor
if keyword_set(dolzr) then clzr

end
