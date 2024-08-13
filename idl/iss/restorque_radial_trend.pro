; Sequence of Prometheus resonance torques taken from restorque_plot.pro
m1 = 2
m2 = 38
num = m2-m1+1
torq = dblarr(num)
rres = dblarr(num)
dd_dr = dblarr(num)
dphi_slm_dr = dblarr(num)
omegares = dblarr(num)
phi_slm = dblarr(num)
patternspeed = dblarr(num)
rname = strarr(num)
mm = intarr(num)
for m=m1,m2 do begin
  ; Prometheus first order
  torq[m-m1] = restorque( m, m-1, 1, res_descrip=res_descrip, /short, $
                          lc82=lc82, rres=_rres, dd_dr=_dd_dr, $
                          dphi_slm_dr=_dphi_slm_dr, omegares=_omegares, $
                          phi_slm=_phi_slm, patternspeed=_patternspeed)
  rres[m-m1] = _rres
  dd_dr[m-m1] = _dd_dr
  dphi_slm_dr[m-m1] = _dphi_slm_dr
  omegares[m-m1] = _omegares
  phi_slm[m-m1] = _phi_slm
  patternspeed[m-m1] = _patternspeed
  rname[m-m1] = res_descrip
  mm[m-m1] = m
endfor

torqueoversigma = -mm * !dpi^2 / ( rres * dd_dr ) * $
       ( rres*dphi_slm_dr + 2*omegares*phi_slm/abs(omegares-patternspeed) )^2

solid_small_circles
plot, tkm(rres), -torq, /ylog, /ys, ps=-8, xtit='Radius'+tkmtit(), ytit='Torque'
print, 'Ranges:'
print, 'Torque: '
print, strtrim(10^!y.crange)
plot, tkm(rres), mm, /ylog, /ys, ps=-8, /noerase, co=ctred()
print, 'm: '
print, strtrim(10^!y.crange)
plot, tkm(rres), 1.0d0/rres, /ylog, /ys, ps=-8, /noerase, co=ctblue()
print, '1/rres: '
print, strtrim(10^!y.crange)
plot, tkm(rres), 1.0d0/dd_dr, /ylog, /ys, ps=-8, /noerase, co=ctgreen()
print, '1/dd_dr: '
print, strtrim(10^!y.crange)
plot, tkm(rres), 1.0d0/( rres*dphi_slm_dr + 2*omegares*phi_slm/abs(omegares-patternspeed) )^2, /ylog, /ys, ps=-8, /noerase, co=ctyellow()
print, '1/x: '
print, strtrim(10^!y.crange)

end
