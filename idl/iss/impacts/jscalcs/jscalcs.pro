function jscalcs_newton_func, xx
  common jscalcs_newton, r1, r2, deltat, deltaa, mu, fix_cap_e
  ee = xx[0]
  pomega = xx[1]
  aa = 2*r1*r2/(r1+r2) / (1-ee^2)
  tt = 2.0d0/3 * aa/deltaa * deltat
  cosp = cos(pomega)
  ecosp = ee*cosp
  zero1 = (1-r1/r2) / (1+r1/r2) - ecosp
  f1 = 2*!dpi - pomega
  f2 = !dpi - pomega
  cap_e1 = acos(( 1 - (1-ee^2) / (1+ee*cos(f1)) )/ee)
  cap_e2 = - acos(( 1 - (1-ee^2) / (1+ee*cos(f2)) )/ee)
  nn = sqrt( mu / aa^3 )
  zero2 = 2*!dpi + cap_e2 - cap_e1 + $
          (1-2*fix_cap_e)*ee*(sin(cap_e2)-sin(cap_e1)) - nn*tt
  return, [zero1,zero2]
end

common jscalcs_newton, _r1, r2, deltat, deltaa, mu, _fix_cap_e

; Knowns and ranges
nr = 11;601
r1 = dindgen(nr)/(nr-1)*60000 + 80000  ; km
r2 = 129380.0d0  ; km
deltat = 35.0d0  ; seconds
deltaa = 100.0d0  ; km
mu = 37931207.7d0  ;km^3/s^2

; Initial guesses
ee = reverse(dindgen(nr))/(nr-1)*.1 + 0.5
pomega = (2-ee)*!dpi

tol = 1d-15
if keyword_exists(fix_cap_e) then _fix_cap_e=fix_cap_e else _fix_cap_e=1

for j=0,nr-1 do begin
  _r1 = r1[j]
  xx = [ee[j],pomega[j]]
  xx = newton( xx, 'jscalcs_newton_func', /double, itmax=500000L, $
               tolx=tol, tolf=tol, tolmin=tol )
  ee[j] = xx[0]
  pomega[j] = xx[1]
endfor
f1 = 2*!dpi - pomega
f2 = !dpi - pomega
cap_e1 = acos(( 1 - (1-ee^2) / (1+ee*cos(f1)) )/ee)
cap_e2 = - acos(( 1 - (1-ee^2) / (1+ee*cos(f2)) )/ee)
aa = 2*r1*r2/(r1+r2) / (1-ee^2)
nn = sqrt( mu / aa^3 )
tt = 2.0d0/3 * aa/deltaa * deltat
vinf2 = 2*mu/r1
vpost2 = vinf2 - mu/aa
deltav = mu/4/aa^2/sqrt(vpost2)*deltaa

!p.multi = [0,2,2]
plot, [80,140], [0,200], /xs, /ys, /nodata, $
      xtit='R1, kkm', ytit='[a,q,Q]=[red,green,blue], kkm'
oplot, tkm(r1), tkm(aa*(1+ee)), co=ctblue()
oplot, tkm(r1), tkm(aa*(1-ee)), co=ctgreen()
oplot, tkm(r1), tkm(aa), co=ctred()
oplot, !x.crange, [136.8,136.8], l=2
plot, [80,140], [0,2], /xs, ys=9, /nodata, xtit='R1, kkm', $
      ytit='[e,pomega/pi]=[blue,red]'
axis, yaxis=1, /ys, yr=!y.crange*180, ytit='pomega, !Uo!N', yticki=90
oplot, tkm(r1), pomega/!dpi, co=ctred()
oplot, tkm(r1), ee, co=ctblue()
if keyword_set(fplot) then begin
  plot, [80,140], [-1,1], /xs, ys=9, /nodata, xtit='R1, kkm', $
        ytit='[f/pi,E/pi]=[solid,dotted], [1,2]=[red,blue], radians'
  axis, yaxis=1, /ys, yr=!y.crange*180, yticki=90, $
        ytit='[f/pi,E/pi]=[solid,dotted], [1,2]=[red,blue], !Uo!N'
  oplot, tkm(r1), f1/!dpi, co=ctred()
  oplot, tkm(r1), f2/!dpi, co=ctblue()
  oplot, tkm(r1), cap_e1/!dpi, co=ctred(), l=1
  oplot, tkm(r1), cap_e2/!dpi, co=ctblue(), l=1
endif
plot, [80,140], [0,35], /xs, /ys, /nodata, xtit='R1, kkm', $
      ytit='[v_inf,v_post,1000*dv]=[red,blue,green], km/s'
oplot, tkm(r1), sqrt(vinf2), co=ctred()
oplot, tkm(r1), sqrt(vpost2), co=ctblue()
oplot, tkm(r1), deltav*1000, co=ctgreen()
plot, tkm(r1), 2.0d0*!dpi/nn/3600, /xs, xtit='R1, kkm', $
      ytit='[P,t]=[red,blue], hours'
oplot, tkm(r1), 2.0d0*!dpi/nn/3600, co=ctred()
oplot, tkm(r1), tt/3600, co=ctblue()
  
end
