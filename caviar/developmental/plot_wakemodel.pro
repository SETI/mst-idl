pro plot_wakemodel, radi, thetaim, thetapan, rpan, wakepredict=wakepredict, $
                    nwakes=nwakes, theta=theta, color=color

if n_params() eq 0 then begin
  print, 'Syntax:  PLOT_WAKEMODEL, radi, thetaim, thetapan, rpan, nwakes='
  print, 'Please enter longitudes in degrees.'
  retall
endif

if not keyword_set(rpan) then rpan = 133584.6
if keyword_exists(thetaim) and keyword_exists(thetapan) then begin
  theta = fltarr(n_elements(radi))
  foo = where( radi lt rpan, count )
  if count gt 0 then theta[foo] = thetaim - thetapan
  foo = where( radi ge rpan, count )
  if count gt 0 then theta[foo] = thetapan - thetaim
endif else theta = 180.
theta = fix_angles( theta, /deg, /to360 )
theta = theta / 180 * !dpi
if not keyword_set(nwakes) then nwakes = 1

wakepredict = 2*rpan*theta/3/(radi-rpan)^2
if nwakes gt 1 then for j=1,nwakes-1 do begin
  wakepredict = [ [wakepredict], [2*rpan*(theta+j*2*!pi)/3/(radi-rpan)^2] ]
endfor

for j=0,nwakes-1 do oplot, tkm(radi), wakepredict[*,j], l=1, color=color

end
