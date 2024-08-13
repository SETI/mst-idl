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

pro fdensity_wave4, rrr, p0, f, pder, xi=xi

; These equations are the ones used by Rosen.

common fdensity_wave33, which_params, m, lambda, lambdasat, rsat, thoukm, $
        rres, rr, yy, rres1, sigma1

cap_g = 6.672e-23  ; km^3/g/s^2
i = complex(0,1)

rr = rrr
sigma = sigma1
rres = rres1
omegares = sqrt(resloc_omega2( rres ))
phi = -m*(lambda-lambdasat)
a = p0[0]
xi_d = p0[1]

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

pder = [ [ f / a ], $
         [ f * 3 * xi^3 / xi_d^4 ] ]

;return, int_tabulated( rr, (f-yy)^2 )

end
