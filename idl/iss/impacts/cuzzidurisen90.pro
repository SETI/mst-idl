;function chandra_s, theta, phi, theta0, phi0, tau, ptheta
;  mu = cos(theta)
;  mu0 = cos(theta0)
;  return, mu*mu0/(mu+mu0)*ejecta_phase(theta)
;end
;
;; Ejecta phase function P(\Theta) from Figure 3 and text from p.475 column 2
;ntheta = 1801
;theta = dindgen(ntheta)/(ntheta-1) * !dpi
;ptheta0 = 1 + cos(1.38*theta)
;ptheta0[where(theta ge !dpi/1.38)] = 0
;ptheta_coeff = !dpi/int_tabulated( theta, ptheta0 )
;ptheta = ptheta_coeff * ptheta0
;plot, theta*180/!dpi, ptheta, /xs, /ys

rr = 130000.0d0
; Free parameters rr, tau, alpha, beta, y0
tau0 = 0.28d0
rsat = 60330.0d0
rprime = rr / rsat
cc = 9400*( 1 - rprime/6 )*( rprime/1.8 )^(-1.2)
alpha_p = 0.6 + 0.4*(1-exp(-tau/.5))
mm = 0.22+( 0.02*alpha^2 + 0.4 )*( 1 - exp(-tau) )
ss = 1+( 0.9 - 0.3*rprime )*( 1 - exp(-tau/(0.1+0.4*rprime)) )
ff = cc*exp(1/ss)*( tau/tau0*exp(-tau/tau0) + 1 - exp(-tau/tau0) )
curly_y = y0 / 1d4 * ff * alpha/alpha_p * exp(-(alpha/alpha_p)^ss/ss) * $
          cos(alpha/2)/cos(alpha_p/2) * (cos(beta))^mm

end
