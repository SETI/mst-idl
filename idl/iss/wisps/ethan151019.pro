;Hi Matt, 
;
;Here are those points we were interested in:
;
;ET :               
;287576096.72907036542892456055
;289438499.33576405048370361328
;291627260.62150520086288452148
;293180372.06716156005859375000
;295235471.26289272308349609375
;
;Daphnis Coordinate :
;155.02815
;152.78293
;150.17590
;148.30903
;145.86369 
;
;I'll update shortly with my results of my polyfit.
;
;-Ethan

et = [ 287576096.72907036542892456055d, 289438499.33576405048370361328, $
       291627260.62150520086288452148, 293180372.06716156005859375000, $
       295235471.26289272308349609375 ]
if keyword_set(randomize) then et = et + randomu(seed,5)*3600*5
lon = [ 155.02815d, 152.78293, 150.17590, 148.30903, 145.86369 ]
etweek = ( et - et[0] )/86400/7

if keyword_set(flip) then begin
  fit = svdfit( lon, etweek, 2, measure_errors=replicate(0.01d0,5), $
                sigma=fit_sigma, chisq=fit_chisq )
  sigma = stddev( etweek - poly(lon,fit) )
endif else begin
  fit = svdfit( etweek, lon, 2, measure_errors=replicate(0.01d0,5), $
                sigma=fit_sigma, chisq=fit_chisq )
  sigma = stddev( lon - poly(etweek,fit) )
endelse

!p.multi = [0,1,2]
plot, lon, etweek, ps=4, xtit='Longitude (!Uo!N)', ytit='Weeks from t=0', $
      yr=[-2,15], /ys
if keyword_set(flip) then begin
  oplot, !x.crange, poly(!x.crange,fit), l=1
  plot, lon, etweek - poly(lon,fit), ps=4, $
        xtit='Longitude (!Uo!N)', ytit='Time Residual (weeks)'
  oplot, !x.crange, [0,0], l=1
  xyouts, mean(!x.crange), .01, align=.5, $
          'Sigma = ' + string(sigma,fo='(F5.3)') + $
          ' weeks = ' + string(sigma*7*24,fo='(F4.2)') + ' hr'
endif else begin
  oplot, poly(!y.crange,fit), !y.crange, l=1
  plot, lon - poly(etweek,fit), etweek, ps=4, yr=[-2,15], /ys, $
        xtit='Longitude Residual (!Uo!N)', ytit='Weeks from t=0'
  oplot, [0,0], !y.crange, l=1
  xyouts, mean(!x.crange), .01, align=.5, $
          'Sigma = '+string(sigma,fo='(F5.3)') + '!Uo!N'
endelse

print, 'Fit: '
print, fit
print, 'Fit_sigma:'
print, fit_sigma
print, 'Fit_sigma * sqrt(fit_chisq/(n-m))'
print, fit_sigma * sqrt(fit_chisq/(5-2))

end
