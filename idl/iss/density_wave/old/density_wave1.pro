; Known quantities include Newton's constant cap_g, phase phi, resonance parameter m,
; satellite orbital radius rsat
; The variables that define the curve are the amplitude A, the radial scale xi, 
; and the damping scale xi_d.
; Amplitude A is a function of satellite mass satmass, resonance location rres, 
; and surface density sigma.
; Radial scale xi is a function of resonance location rres, surface density sigma. 
; Note that resonance mean motion omegares is a directly calculable function of
; resonance location rres.

if not keyword_set(m) then m=200
if not keyword_set(moon) then moon=10
cap_g = 6.672e-23  ; km^3/g/s^2
satnames = [ 'Atlas', 'Prometheus', 'Pandora', 'Epimetheus', 'Janus', $
             'Mimas', 'Enceladus', 'Titan', 'Hyperion', 'Iapetus', 'Pan', $
             'Tethys', 'Dione', 'Rhea', 'Phoebe' ]
; Satellite mass values in km^3/s^2, from kernel cpck05May2004.tpc
satmass = [ 0.00072, 0.022, 0.013, 0.0357, 0.1284, 2.54, 6.98, 8978.01, 0.72, $
            132.212, 0.00018, 41.21, 73.12, 154.7, 0.485 ] / cap_g
satmass = satmass[moon]
rres = resloc(m,m,moon,rsat=rsat,omegares=omegares,res_descrip=res_descrip)
thoukm = long( rres - (rres mod 1000) )
if not keyword_set(sigma) then sigma = 43.8 * 1e10  ; g/km^2

;a = .2
;xi = findgen(1001) / 100
;r = rres*xi * sqrt( 4*!pi*cap_g*sigma / (3*(m-1)*omegares^2*rres) ) + rres

if not keyword_set(nr) then nr = 1001
if not keyword_set(deltar) then deltar = 40
r = findgen(nr) / deltar + rres - thoukm
xi = sqrt( 3*(m-1)*omegares^2*rres / (4*!pi*cap_g*sigma) ) * (r+thoukm-rres) / rres

i = complex(0,1)
xi_d = 4
if not keyword_set(phi) then phi = 0 ;radians

alpha = rres / rsat
; The outputs of the laplace function are as follows:
; lapl[0,m,0] = B(1/2,m)
; lapl[0,m,1] = a*dB(1/2,m)/da
lapl = laplace( alpha, 0, m, 1 )
a = satmass / ( 2 * sqrt(!dpi) * rsat * rres * sigma ) * $
    ( lapl[0,m,1] + 2*m*lapl[0,m,0] )

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

tau_norm = 1 + real_part( -i*a*(!pi^(-1./2)-2*i*xi*h)*exp(i*phi) )*exp( (-xi/xi_d)^3 )

plot, r, tau_norm, /xs, /ys, xtit='Radius - '+strtrim(thoukm,2)

end
