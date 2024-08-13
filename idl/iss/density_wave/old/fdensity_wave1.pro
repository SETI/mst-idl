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

pro fdensity_wave1, r, aa, f, pder

; Known quantities include Newton's constant cap_g, phase phi, resonance parameter m,
; satellite orbital radius rsat
; The variables that define the curve are the amplitude A, the radial scale xi, 
; and the damping scale xi_d.
; Amplitude A is a function of satellite mass satmass, resonance location rres, 
; and surface density sigma.
; Radial scale xi is a function of resonance location rres, surface density sigma. 
; Note that resonance mean motion omegares is a directly calculable function of
; resonance location rres.

common fdensity_wave11, m, phi, rsat, thoukm, rres

cap_g = 6.672e-23  ; km^3/g/s^2
;rres = aa[3]
omegares = sqrt(resloc_omega2( rres, domegadrres ))
sigma = aa[0]
satmass = aa[1]
xi_d = aa[2]

xi = sqrt( 3*(m-1)*omegares^2*rres / (4*!pi*cap_g*sigma) ) * (r+thoukm-rres) / rres
dxidsigma = -0.5 * xi / sigma
dxidrres = xi * ( domegadrres/omegares + (r+rres)/(r-rres)/2/rres )

i = complex(0,1)

alpha = rres / rsat
; The outputs of the laplace function are as follows:
; lapl[0,m,0] = B(1/2,m)
; lapl[0,m,1] = a*dB(1/2,m)/da
lapl = laplace( alpha, 0, m, 1 )
a = satmass / ( 2 * sqrt(!dpi) * rsat * rres * sigma ) * $
    ( lapl[0,m,1] + 2*m*lapl[0,m,0] )
dadsatmass = a / satmass
dadrres = -a / rres
dadsigma = -a / sigma

; The fresnel_int function actually computes the integral from -Inf to x of 
; exp(-i*!pi*x^2/2) dx, rather than exp(-i*x^2) dx.  To correct for this, divide
; the input x by sqrt(!pi/2) and multiply the result by the same factor. 
; To check that this works:
; plot, xi, deriv( xi, sqrt(!pi/2)*fresnel_int( xi*sqrt(2/!pi) ) )
; oplot, xi, cos(xi^2), co=ctcyan()
; The two curves should be identical.
h = !pi^(-1./2) * exp(-i*xi^2) * $
                     complex( sqrt(!dpi/2)*fresnel_int(xi*sqrt(!dpi/2)), $
                              sqrt(!dpi/2)*fresnel_int(xi*sqrt(!dpi/2),/sine) )
; Note that the derivative of the Fresnel factor is exp(i*xi^2), which cancels with
; the exponential outside the Fresnel factor.
dhdxi = -2*i*xi*h + !pi^(-1./2)

f = 1 + real_part( -i*a*(!pi^(-1./2)-2*i*xi*h)*exp(i*phi) )*exp( (-xi/xi_d)^3 )
dfdxi = -3 * xi^2 / xi_d^3 * (f-1) + $
        real_part( -2*a*( h + xi*dhdxi )*exp(i*phi) )*exp( (-xi/xi_d)^3 )
dfdxi_d = 3 * xi^3 / xi_d^4 * (f-1)
dfda = (f-1) / a

;pder = [ [ dfdxi*dxidrres + dfda*dadrres ], $
;         [ dfdxi*dxidsigma + dfda*dadsigma ], $
;         [ dfda*dadsatmass ], $
;         [ dfdxi_d ] ]
pder = [ [ dfdxi*dxidsigma + dfda*dadsigma ], $
         [ dfda*dadsatmass ], $
         [ dfdxi_d ] ]

end
