; Compute the "u" value = (x/a)^2 + (y/b)^2 with optional rotation
function mpfit2dpeak_uprop, x, y, p, dp, tilt=tilt, symmetric=sym
  widx  = abs(p(2)) > 1e-20 & widy  = abs(p(3)) > 1e-20 
  if keyword_set(sym) then widy = widx
  xp    = x-p(4)            & yp    = y-p(5)
  sz = size(x)
  return, (xp/widx)^2 + (yp/widy)^2

end

; Propeller Function
; For use with mpfit_propeller.pro
;      A(0)   Constant baseline level
;      A(1)   Peak value
;      A(2)   Peak half-width (x) -- gaussian sigma or half-width at half-max
;      A(3)   Peak half-width (y) -- gaussian sigma or half-width at half-max
;      A(4)   Center of complex (x)
;      A(5)   Center of complex (y)
;      A(6)   Separation between peak edges (x)
;      A(7)   Separation between peak mid-lines (y)
function mpfit2dpeak_propeller1, x, y, p, dp, tilt=tilt, symmetric=sym, _extra=extra
  sz = size(x)
  if sz(sz(0)+1) EQ 5 then smax = 26D else smax = 13.

  ; Don't allow one lobe of the propeller to be off the edge of the image.
  if p[4] lt 0 then p[4] = 0
  if p[4] gt max(x) then p[4] = max(x)
  if p[5] lt 0 then p[5] = 0
  if p[5] gt max(y) then p[5] = max(y)
;  if p[2] gt max(x)/3 then p[2] = max(x)/3
;  if p[6] lt p[4]-p[2] then p[6] = p[4]-p[2]
;  if p[6] gt max(x)-p[4]-p[2] then p[6] = max(x)-p[4]-p[2]

  p1 = [ p[0:3], p[4]-p[6]-p[2], p[5]+p[7], 0, 0 ]
  p2 = [ p[0:3], p[4]+p[6]+p[2], p[5]-p[7], 0, 0 ]
  ;p1 = [ p[0:3], p[4]-p[6]-p[2], p[5]+p[3]-p[7], 0, 0 ]
  ;p2 = [ p[0:3], p[4]+p[6]+p[2], p[5]-p[3]+p[7], 0, 0 ]
  u1 = mpfit2dpeak_uprop(x, y, p1, dp1p, tilt=0, symmetric=keyword_set(sym))
  u2 = mpfit2dpeak_uprop(x, y, p2, dp2p, tilt=0, symmetric=keyword_set(sym))
  mask1 = u1 LT (smax^2)  ;; Prevents floating underflow
  mask2 = u2 LT (smax^2)  ;; Prevents floating underflow
  out1 = p(0) + p(1) * mask1 * exp(-0.5 * u1 * mask1)
  out2 = p(0) + p(1) * mask2 * exp(-0.5 * u2 * mask2)
  return, out1 + out2
end
