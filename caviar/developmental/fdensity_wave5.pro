function fdensity_wave5, rr, a=a, xi_d=xi_d, mm=mm, phi=phi, rres=rres, sigma=sigma, xiout=xi, pder=pder, fresnel_integral=fresnel_integral, noinward=noinward

; These equations are the ones used by Rosen.
; See ~/idl/iss/density_wave/fdensity_wave4.pro for previous version.

if not keyword_exists(noinward) then noinward = 1
cap_g = 6.672e-23  ; km^3/g/s^2
i = complex(0,1)

sigma1 = sigma * 1e10
omegares = sqrt(caviar_omega2( rres ))

xi = 3*(mm-1)*omegares^2*rres / (2*!pi*cap_g*sigma1) 
if xi lt 0 then stop, 'Heading for a crash.'
xi = sqrt(xi) * (rr-rres) / rres

; The fresnel_int function actually computes the integral from -Inf to x of 
; exp(-i*!pi*x^2/2) dx, rather than exp(-i*x^2/2) dx.  To correct for this, 
; divide the input x by sqrt(!pi) and multiply the result by the same factor. 
; To check that this works:
; plot, xi, deriv( xi, sqrt(!pi)*fresnel_int( xi/sqrt(!pi) ) )
; oplot, xi, cos(xi^2), co=ctcyan()
; The two curves should be identical.
fresnel_integral = complex(sqrt(!dpi) * fresnel_int( xi/sqrt(!dpi) ), $
                           sqrt(!dpi) * fresnel_int( xi/sqrt(!dpi), /sine ))

; Previous versions of this program missed the minus sign in -i*phi
f = real_part( i*a * exp(-i*phi*!dpi/180) * $
	( 1 - i*xi * exp(-i*xi^2/2) * fresnel_integral ) )
if keyword_set(xi_d) then f = f * exp( -(xi/xi_d)^3 )

pder = [ [ f / a ], $
         [ f * 3 * xi^3 / xi_d^4 ] ]

if keyword_set(noinward) then begin
  foo = where( rr lt rres, count )
  if count gt 0 then f[foo] = 0
endif

return, f

end
