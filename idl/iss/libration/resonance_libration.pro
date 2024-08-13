ee = .0196d0
edot = 0.0d0
fd = 5.7141d0
mratio = 2.5d0/37929085
nn = 8.9991d-5
alpha = .9027d0
cr = -mratio*nn*alpha*fd  
j = 7
ndot = 3*j*cr*nn*ee ;*sin(phi)
pomegadot1 = -cr/ee^2*edot ;*cos(phi)
pomegadot2 = -cr/ee ;*phidot*sin(phi)

nt = 100001l
dt = 86400.0d0 * .1
tt = dindgen(nt) * dt
phi = dblarr(nt)
phidot = dblarr(nt)
phiddot = dblarr(nt)
phi[0] = !dpi / 180 / 10
for k=1l,nt-1 do begin
  phiddot[k-1] = j*ndot*sin(phi[k-1])
;  phiddot[k-1] = j*ndot*sin(phi[k-1]) + $
;                 pomegadot1*cos(phi[k-1]) + $
;                 pomegadot2*phidot[k-1]*sin(phi[k-1])
  phi[k] = phi[k-1] + phidot[k-1]*dt
  phidot[k] = phidot[k-1] + phiddot[k-1]*dt
endfor

!p.multi = [0,1,2]
plot, tt/86400, phi*180/!dpi, /xs, /ys, xtit='Time (days)', ytit='Phi'
run_fft, phi, power, f, dx=dt/86400

end

