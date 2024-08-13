if keyword_set(findfile('seed.sav')) then restore, 'seed.sav' else begin
  foo = randomu(seed)
  save, seed, filename='seed.sav'
endelse
Omega = sqrt(caviar_omega2(130000.0d0))  ; in radians/s, for a = 130000 km
np = 10000
yrfac = 1
case var of
  1: begin
    ; The case in Schmidt's 11/10/10 memo
    qq = 2.2d0
    cmin = 0.01d0   ; m/s
    cmax = 100.0d0  ; m/s
    x0 = dblarr(np)
    y0 = dblarr(np)
    z0 = dblarr(np)
    cc = cmin*( ( (cmax/cmin)^(1-qq) - 1 )*randomu(seed,np) + 1 )^(1/(1-qq))
    theta = acos( 1 - 2*randomu(seed,np) )
    phi = 2*!dpi*randomu(seed,np)
    u0 = cc*sin(theta)*cos(phi)
    v0 = cc*sin(theta)*sin(phi)
    w0 = cc*cos(theta)
  end
  2: begin
    ; Like Schmidt, except constant velocity
    qq = 1
    cmin = 1.0d0   ; m/s
    cmax = 10.0d0  ; m/s
    x0 = dblarr(np)
    y0 = dblarr(np)
    z0 = dblarr(np)
    cc = cmin;*( ( (cmax/cmin)^(1-qq) - 1 )*randomu(seed,np) + 1 )^(1/(1-qq))
    theta = acos( 1 - 2*randomu(seed,np) )
    phi = 2*!dpi*randomu(seed,np)
    u0 = cc*sin(theta)*cos(phi)
    v0 = cc*sin(theta)*sin(phi)
    w0 = cc*cos(theta)
  end
  3: begin
    ; Zero velocity (except keplerian shear), dispersed positions
    rr = 1e4*randomu(seed,np)
    phi = 2*!dpi*randomu(seed,np)
    x0 = rr*cos(phi)
    y0 = rr*sin(phi)
    z0 = dblarr(np)
    u0 = dblarr(np)
    v0 = -1.5d0*x0*Omega
    w0 = dblarr(np)
    yrfac = 0.2
  end
endcase

if keyword_set(usecolors) then begin
  _red = where( u0/v0 ge 0 and u0/v0 lt 1/sqrt(3) )
  _green = where( u0/v0 ge 1/sqrt(3) and u0/v0 lt sqrt(3) )
  _blue = where( u0/v0 ge sqrt(3) )
  _yellow = where( u0/v0 lt -sqrt(3) )
  _purple = where( u0/v0 ge -sqrt(3) and u0/v0 lt -1/sqrt(3) )
  _cyan = where( u0/v0 ge -1/sqrt(3) and u0/v0 lt 0 )
endif
nt = 80
xx = dblarr(np,nt)
yy = dblarr(np,nt)
zz = dblarr(np,nt)
Omegat = dblarr(nt)
for j=0,nt-1 do begin
  Omegat[j] = 2*!dpi*(j+1)/10
  xx[*,j] = 4*x0 + 2*v0/Omega - (3*x0+2*v0/Omega)*cos(Omegat[j]) + u0/Omega*sin(Omegat[j])
  yy[*,j] = y0 - 2*u0/Omega - 3*Omegat[j]*(2*x0+v0/Omega) + $
       (6*x0+4*v0/Omega)*sin(Omegat[j]) + 2*u0/Omega*cos(Omegat[j])
  zz[*,j] = z0*cos(Omegat[j]) + w0/Omega*sin(Omegat[j])
  plot, yy[*,j], xx[*,j], ps=3, xr=[-1e5,1e5], yr=[-1e5,1e5]*yrfac
  if keyword_set(usecolors) then begin
    oplot, yy[_red,j], xx[_red,j], ps=3, co=ctred()
    oplot, yy[_green,j], xx[_green,j], ps=3, co=ctgreen()
    oplot, yy[_blue,j], xx[_blue,j], ps=3, co=ctblue()
    oplot, yy[_yellow,j], xx[_yellow,j], ps=3, co=ctyellow()
    oplot, yy[_purple,j], xx[_purple,j], ps=3, co=ctpurple()
    oplot, yy[_cyan,j], xx[_cyan,j], ps=3, co=ctcyan()
  endif 
  wait, .1
endfor

end

