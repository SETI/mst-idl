function resloc_omega2, r, domegadr
  ; Saturn mass, radius, and harmonics from JPL kernel cpck05May2004.tpc
  gm = 37931289.4836 ;km^3/s^2
  prad = 60330.0d0 ;km
  j2 = 0.016292243237d
  j4 = -0.000928716077d
  j6 = 0.000088845313
  omega = gm / r^3 * ( 1 + j2*3/2*(prad/r)^2 - j4*15/8*(prad/r)^4 + j6*35/16*(prad/r)^6 )
  domegadr = 3 * gm / r^4 * ( -1 - j2*5/2*(prad/r)^2 + j4*35/8*(prad/r)^4 - j6*105/16*(prad/r)^6 )
  return, omega
end

function fdensity_wave3, p0, f=f

; These equations are the ones used by Rosen.

common fdensity_wave33, which_params, m, lambda, lambdasat, rsat, thoukm, $
        rres, rr, yy, rres1, sigma1

cap_g = 6.672e-23  ; km^3/g/s^2
i = complex(0,1)

if which_params[0] eq 1 then a = p0[0]
if which_params[1] eq 1 then sigma = p0[1] else sigma = sigma1
if which_params[2] eq 1 then rres = p0[2] else rres = rres1
omegares = sqrt(resloc_omega2( rres ))
if which_params[3] eq 1 then phi = p0[3] else phi = -m*(lambda-lambdasat)
if which_params[4] eq 1 then bg = p0[4]
if keyword_set(bg) then yy = yy - bg
if which_params[5] eq 1 then xi_d = p0[5]

xi = 3*(m-1)*omegares^2*rres / (2*!pi*cap_g*sigma) 
if xi lt 0 then stop, 'Heading for a crash.'
xi = sqrt(xi) * (rr+thoukm-rres) / rres

; The fresnel_int function actually computes the integral from -Inf to x of 
; exp(-i*!pi*x^2/2) dx, rather than exp(-i*x^2/2) dx.  To correct for this, 
; divide the input x by sqrt(!pi) and multiply the result by the same factor. 
; To check that this works:
; plot, xi, deriv( xi, sqrt(!pi)*fresnel_int( xi/sqrt(!pi) ) )
; oplot, xi, cos(xi^2), co=ctcyan()
; The two curves should be identical.
fresnel_integral = complex(sqrt(!dpi) * fresnel_int( xi/sqrt(!dpi) ), $
                           sqrt(!dpi) * fresnel_int( xi/sqrt(!dpi), /sine ))

f = real_part( i*a * exp(i*phi) * $
	( 1 - i*xi * exp(-i*xi^2/2) * fresnel_integral ) )
if keyword_set(xi_d) then f = f * exp( -(xi/xi_d)^3 )
;return, max(abs( f - yy ))
return, int_tabulated( rr, (f-yy)^2 )

end
