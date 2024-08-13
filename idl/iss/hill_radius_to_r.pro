function hill_radius_to_r, rhill, a=aa, density=density, debug=debug

if n_params() eq 0 then begin
  print, 'Syntax:  Result = HILL_RADIUS_TO_R( rhill, a=a, density=density )'
  print, 'All quantities in units of grams and centimeters'
  stop
endif

cap_g = 6.674d-8  ; cm^3 / g / s^2
saturn_constants, gm=gm_sat
if not keyword_set(aa) then aa = 1.3d10
if not keyword_set(density) then density = 0.5d0
gm_sat = gm_sat * 1d15  ; Convert from km^3/s^2 to cm^3/s^2
gm = 3 * gm_sat * (rhill/aa)^3
mass = gm / cap_g
rr = ( mass * 3 / 4 / !dpi / density )^(1.0d0/3)

if keyword_set(debug) then stop

return, rr

end
