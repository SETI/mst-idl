function jscalcs2_newton_func, xx
  common jscalcs2_newton, bb, alpha
  bbprime = xx[0]
  rr = xx[1]
  zero1 = 1 + sqrt(2)*rr*cos(bb) - sqrt(2)*alpha*(1+rr)*cos(bbprime)
  zero2 = cos(bbprime) + sqrt(2)*rr*cos(bb-bbprime) - sqrt(2)*alpha*(1+rr)
  return, [zero1,zero2]
end

common jscalcs2_newton, _bb, alpha
tol = 1d-15

bb = dindgen(181)*!dpi/180
nb = 181
alpha = 0.8d0
sigma = 500d0
rho = 1000d0
bbprime = bb*0
rr = bb*0
for j=0,nb-1 do begin
  _bb = bb[j]
  xx = [ _bb, alpha/( 1 - alpha ) ]
  xx = newton( xx, 'jscalcs2_newton_func', /double, itmax=500000L, $
               tolx=tol, tolf=tol, tolmin=tol )
  bbprime[j] = xx[0]
  rr[j] = xx[1]
endfor

if keyword_set(jscalcs2_check) then begin
  !p.multi = [0,2,2]
  plot, bb*180/!dpi, (sin(bb)-alpha*sin(bbprime))/alpha/sqrt(2), xtit='B (!Uo!N)', tit='(sinB-alpha*sinB'')/alpha/sqrt(2) in black,!Csin(B-B'') in red'
  oplot, bb*180/!dpi, cos(bbprime)*sin(bb) - cos(bb)*sin(bbprime), co=ctred()
  plot, bb*180/!dpi, rr, xtit='B (!Uo!N)', tit='r in black,!Calpha*sinB''/( sinB - alpha*sinB'' ) in red'
  oplot, bb*180/!dpi, alpha*sin(bbprime)/( sin(bb) - alpha*sin(bbprime) ), co=ctred()
endif

!p.multi = [0,1,3]
!y.margin = 0
!y.omargin = [4,2]
notn = replicate(' ',20)

plot, bb*180/!dpi, bb*180/!dpi, l=2, xtickn=notn, ytit='B'' (!Uo!N)', xticki=45, yticki=45
oplot, bb*180/!dpi, bbprime*180/!dpi
plot, [0,180], [0,3], /xs, /ys, /nodata, xtickn=notn, ytit='R_projectile (m)', xticki=45
clr = [ctred(),ctgreen(),ctblue()]
for j=0,2 do begin
  ff = .1+.4*j
  cap_r = rr*(1-ff)*3*sigma/4/rho
  oplot, bb*180/!dpi, cap_r, co=clr[j]
endfor
plot, [0,180], [.68,1], /xs, /ys, /nodata, xtickn=notn, ytit='E_out/E_in', xticki=45
for j=0,2 do begin
  ff = .1+.4*j
  e1e2 = ( ff + 2*(1+rr)*(1-ff)*alpha^2 )/( 1 + 2*(1-ff)*rr )
  oplot, bb*180/!dpi, e1e2, co=clr[j]
endfor

!y.margin = [4,2]
!y.omargin = 0

end
