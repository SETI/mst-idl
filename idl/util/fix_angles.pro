function fix_angles, _theta, rad=rad, deg=deg, to360=to360

; For angles that have cycled beyond 360 degrees, unwraps so that it is in 
; the range of -180 to 180 (or 0 to 360 if keyword to360 is set).  Default
; is degrees, keyword rad forces conversion in radians.

if n_params() eq 0 then begin
  print, 'Syntax:  angle = fix_angles( _angle, /rad, /deg, /to360 )'
  return, -1
endif

theta = _theta
if keyword_set(rad) then theta = theta * 180/!dpi
theta = theta mod 360
if keyword_set(to360) then begin
  foo = where(theta lt 0, count)
  if to360 eq 2 then foo = where(theta le 0, count)
  if count gt 0 then theta[foo] = theta[foo] + 360
endif else begin
  foo = where(theta le -180, count)
  if count gt 0 then theta[foo] = theta[foo] + 360
  foo = where(theta gt 180, count)
  if count gt 0 then theta[foo] = theta[foo] - 360
endelse
if keyword_set(rad) then theta = theta * !dpi/180

return, theta

end
