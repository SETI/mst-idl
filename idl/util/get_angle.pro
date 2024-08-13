function get_angle, sin_theta, cos_theta, rad=rad, to360=to360, $
                    normalize=normalize

; For input sin_theta and cos_theta, finds the angle theta.

if n_params() eq 0 then begin
  print, 'Syntax:  Result = GET_ANGLE( sin_theta, cos_theta, /rad, /to360 )'
  return, -1
endif

n = n_elements(cos_theta)
if keyword_set(normalize) then begin
  r2 = double(sin_theta)^2 + double(cos_theta)^2
endif else r2 = 1.0d0
sin_theta = double(sin_theta) / sqrt(r2)

x = where( cos_theta ge 0, count )
if count eq 0 then begin
  theta = !dpi - asin(sin_theta)
endif else if count eq n then begin
  theta = asin(sin_theta)
endif else begin
  y = vec_remove(lindgen(n),x)
  theta = dblarr(n)
  theta[x] = asin(sin_theta[x])
  theta[y] = !dpi - asin(sin_theta[y])
endelse
if not keyword_set(rad) then theta = theta * 180 / !dpi
theta = fix_angles( theta, rad=rad, to360=to360 )

return, theta

end
