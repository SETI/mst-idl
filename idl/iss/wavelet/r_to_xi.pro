function r_to_xi, rr, rres, mm, sigma

if n_params() eq 0 then begin
  print, 'Syntax:  Result = R_TO_XI( r, rres, m, sigma )'
  print, 'Calculates the dimensionless radial parameter xi.'
  print, 'r and rres should be in km, sigma in g/cm^2'
  retall
endif

omegares = sqrt(caviar_omega2( rres ))   ; rad/s
_sigma = sigma * 1e10  ; convert to g/km^2
cap_g = 6.672e-23      ; km^3 / g / s^2

out = 3 * (mm-1) * omegares^2 * rres / 2 / !dpi / cap_g / _sigma
out = sqrt(out) * ( rr - rres ) / rres

return, out

end
