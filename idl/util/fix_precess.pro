function fix_precess, pomega, nrange=nrange, to360=to360, debug=debug, sm=sm

pomega = fix_angles( pomega, to360=to360 )
npomega = n_elements(pomega)
pomegadot = median(deriv( pomega ))
pomega_fixed = pomega
if not keyword_set(nrange) then nrange = 7
range = indgen(nrange) - (nrange-1)/2
if not keyword_set(sm) then sm = 1
for j=1l,npomega-1 do begin
  delta = pomega_fixed[j] - median(pomega_fixed[(j-sm)>0:j-1])
  x = where( abs(delta-range*180) eq min(abs(delta-range*180)) )
  if x eq 0 or x eq nrange-1 then stop, 'Need a bigger range'
  pomega_fixed[j] = fix_angles( pomega_fixed[j] - range[x]*180, to360=to360 )
  if keyword_exists(debug) then if j eq debug then stop
endfor

return, pomega_fixed

end
