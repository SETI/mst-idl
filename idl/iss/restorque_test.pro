t_llm = dblarr(10)
x_nl = dblarr(10)
x_max = dblarr(10)
dd_dr = dblarr(10)
rres = dblarr(10)
omegares = dblarr(10)
phi_slm = dblarr(10)
dphi_slm_dr = dblarr(10)
for j=0,9 do begin
  t_llm[j] = restorque( j+2, j+1, 616, /lc82, x_nl=_x_nl, x_max=_x_max, $
                        rres=_rres, dd_dr=_dd_dr, patternspeed=patternspeed, $
                        omegares=_omegares, $
                        phi_slm=_phi_slm, dphi_slm_dr=_dphi_slm_dr )
  x_nl[j] = _x_nl
  x_max[j] = _x_max
  dd_dr[j] = _dd_dr
  rres[j] = _rres
  omegares[j] = _omegares
  phi_slm[j] = _phi_slm
  dphi_slm_dr[j] = _dphi_slm_dr
endfor
t_llm_lc82 = [ -8.39e15,-3.11e16,-6.81e16,-1.2e17,-1.86e17,-2.66e17,-3.61e17,$
               -4.7e17,-5.95e17,-7.33e17 ]
x_nl_lc82 = [ 3e-3, 1.83e-3, 1.25e-3, 9.17e-4, 7.09e-4, 5.69e-4, 4.7e-4, $
              3.96e-4, 3.4e-4, 2.95e-4 ]
x_max_lc82 = [ 2.35e-2, 3.59e-2, 4.83e-2, 6.07e-2, 7.31e-2, 8.55e-2, 9.79e-2, $
               1.1e-1, 1.23e-1, 1.35e-1 ]

; The numbers you get if you use the LC82 value for torque, instead of t_llm
; These are now reasonably close to _prime/_lc82 = 1
x_nl_prime = x_nl * sqrt(-t_llm) / sqrt(-t_llm_lc82)
x_max_prime = x_max / sqrt(-t_llm) * sqrt(-t_llm_lc82)
g_sigma100 = 6.672e-8*100       ; G * 100 g/cm^2, in cm / s^2
mm = findgen(10)+2
dd_dr_prime = 2*!dpi^2 / x_nl_lc82 * sqrt(-mm/rres/t_llm_lc82) * g_sigma100^(3./2)

fit = poly_fit( mm, t_llm/t_llm_lc82, 2 )  ; Very good fit.
plot, mm, t_llm/t_llm_lc82, ps=-4
oplot, mm, poly(mm,fit), co=ctred()



end
