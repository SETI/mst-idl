function wavelet_ridgepower, wave, ridges, minpower=minpower, nosqrt=nosqrt, $
  doplot=doplot, gauss=gauss

if n_params() eq 0 then begin
  print, 'Syntax:  Result = WAVELET_RIDGEPOWER( wave, ridges )'
  print, 'For input arrays wave and ridges, finds the value of the wavelet power at each ridge location.  Default simply interpolates at the ridge location.  If minpower is set, then instead integrates the interval centered on each ridge location in which the power is greater than minpower.'
  retall
endif

power = abs(wave)
if not keyword_exists(nosqrt) then nosqrt = 1
if keyword_set(nosqrt) then power = power^2
ridgepower1 = interpolate( power, ridges[*,0], ridges[*,1] )
if keyword_set(minpower) then begin
  ridgepower = ridgepower1 * 0
  for j=0,n_elements(ridgepower1)-1 do begin
    if ridgepower1[j] ge minpower then begin
      smaller = where( power[ridges[j],*] lt minpower, count )
      if count eq 0 then begin
stop
      endif else begin
        ; Get lower bound of the integration
        foo = where( smaller lt ridges[j,1], count )
        if count gt 0 then bound1 = max(smaller[foo])
        int = lindgen( fix(ridges[j,1]) - bound1 + 1 ) + bound1
        bound1 = interpol( [int,ridges[j,1]], $
                           [[power[j,int]],[ridgepower1[j]]], minpower )
        ; Get upper bound of the integration
        foo = where( smaller gt ridges[j,1], count )
        if count gt 0 then bound2 = min(smaller[foo])
        int = lindgen( bound2 - ceil(ridges[j,1]) + 1 ) + ceil(ridges[j,1])
        bound2 = interpol( [ridges[j,1],int], $
                           [[ridgepower1[j]],[power[j,int]]], minpower )
        if fix(bound2) gt fix(bound1) then begin
          int = lindgen( fix(bound2) - ceil(bound1) + 1 ) + ceil(bound1)
          ridgepower[j] = int_tabulated( [bound1,int,bound2], $
                               [minpower,reform(power[j,int]),minpower] )
          ridgepower[j] = ridgepower[j] / (bound2-bound1)  ; Average value
          if keyword_set(doplot) then begin
            plot, lindgen(n_elements(wave[0,*])), power[j,*], /xs, /ys, $
                  yr=[0,max(power)]
            oplot, [bound1,int,bound2], $
                   [minpower,reform(power[j,int]),minpower], co=red()
            oplot, [ridges[j,1]], [ridgepower1[j]], ps=4, co=red()
            wait, doplot
          endif
        endif
      endelse
    endif
  endfor
;endif else if keyword_set(gauss) then begin
;  ridgepower = ridgepower1 * 0
;  for j=0,n_elements(ridgepower1)-1 do begin
;stop
;;    ridgepower[j] = 
;  endfor
endif else begin
  ridgepower = ridgepower1
endelse

return, ridgepower

end
