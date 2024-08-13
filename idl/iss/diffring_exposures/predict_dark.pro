function predict_dark, instrument, time, gain, sum, silent=silent

; Calculate dark currents per Cisscal's 2-parameter model 
; (no longer in use, but perhaps useful for pre-estimates?)
; Time is in milliseconds, gain is an integer 0 to 3, sum is 1, 2, or 4.

if instrument eq 'WAC' then begin
  meandark = 2.33
  meandarkderiv = .453
  gainratios = [ 0.125446, 0.290637, 1.0, 2.360374 ]
endif else begin
  meandark = 1.56
  meandarkderiv = .430
  gainratios = [ 0.135386, 0.309569, 1.0, 2.357285 ]
endelse
if not keyword_set(time) then time = 1
if not keyword_set(gain) then gain = 3
if not keyword_set(sum) then sum = 2

darkdn = ( meandark + meandarkderiv*time ) * gainratios[gain] * sum^2
if not keyword_set(silent) then print, darkdn

return, darkdn

end


