function unwrap_phase, phase, wrap1=wrap, wrapsign=wrapsign, radians=radians, debug=debug, noneg=noneg

if n_params() eq 0 then begin
  print, 'Syntax:  Result = UNWRAP_PHASE( phase, wrap=, wrapsign= )'
  retall
endif

if keyword_set(radians) then qq=2*!dpi else qq=360
nph = n_elements(phase)
wrap = get_phase_wrap( phase, wrapsign=wrapsign, radians=radians )
if keyword_set(noneg) then begin
  if abs(noneg) ne 1 then begin
    stop, 'abs(noneg) should be 1'
    foo = where( wrapsign eq -noneg, count )
    foo1 = where( abs( phase[wrap[foo]] - phase[wrap[foo]-1] - qq*noneg ) gt $
                  10*median(deriv(phase)), count1 )
    if count1 ne 0 then stop, 'Input is not monotonic.'
  endif
endif
if keyword_set(wrapsign) then wrapsign_tot = total( wrapsign, /cumulative )

out = phase
for jj=1l,n_elements(wrap)-2 do begin
  out[wrap[jj]:wrap[jj+1]-1] = out[wrap[jj]:wrap[jj+1]-1] + qq*wrapsign_tot[jj]
endfor
if keyword_set(debug) then stop

return, out

end
