Omegat = findgen(10000)/9999*8*!pi

tanphi1 = 2*( 5*sin(Omegat) - 3*Omegat )
tanphi2 = sqrt(2*( 73 + 18*Omegat^2 - 64*cos(Omegat) - $
                   9*cos(2*Omegat) - 60*Omegat*sin(Omegat) ))
tanphi_schmidt = (tanphi1-tanphi2)/( 16*(sin(Omegat/2))^2 )
tanphi_schmidt = -1/tanphi_schmidt  ; Convert to my preferred convention

phi0 = 0
tanphi = ( -.5*sin(phi0)*sin(Omegat) + cos(phi0)*(1-cos(Omegat)) )/$
         ( -sin(phi0)*(1-cos(Omegat)) + cos(phi0)*(1.5*Omegat-2*sin(Omegat)) )
ft = ( 8 - 8*cos(Omegat) - 3*Omegat*sin(Omegat) )/$
     ( 9*Omegat^2 - 21*Omegat*sin(Omegat) + 12*sin(Omegat)^2 )

if keyword_set(dolzr) then begin
  lzr, 'cantanglecalculations1'
  @plot_prepare
  plot_color
endif
!p.multi = [0,2,2]
plot, /xs, /ys, Omegat/2/!pi, atan(tanphi)*180/!pi, yr=[0,10], $
      xtit='Orbits', ytit='Cant Angle (!Uo!N) for!Cisotropic initial velocities'
x0 = !x.crange[0]
dx = !x.crange[1]-!x.crange[0]
y0 = !y.crange[0]
dy = !y.crange[1]-!y.crange[0]
;oplot, x0+dx*[.5,.57], y0+dy*.915*[1,1]
xyouts, x0+dx*.4, y0+dy*.9, 'Tiscareno (10 Oct 2011), Equation 16', chars=1
oplot, Omegat/2/!pi, atan((1-cos(Omegat))/(1.5*Omegat-2*sin(Omegat)))*180/!pi, $
       co=ctred()
;oplot, x0+dx*[.5,.57], y0+dy*.865*[1,1], co=ctred()
xyouts, x0+dx*.4, y0+dy*.85, 'Tiscareno (10 Oct 2011), Equation 15', chars=1, $
        co=ctred()
oplot, Omegat/2/!pi, atan(tanphi_schmidt)*180/!pi, co=ctgreen()
;oplot, x0+dx*[.5,.57], y0+dy*.815*[1,1], co=ctgreen()
xyouts, x0+dx*.4, y0+dy*.8, 'Schmidt (19 Sept 2011), Equation 32', chars=1, $
        co=ctgreen()
oplot, Omegat/2/!pi, atan(tanphi)*180/!pi

;!p.multi[0] = !p.multi[0] - 1
plot, /xs, /ys, Omegat/2/!pi, (atan(tanphi)-tanphi)*180/!pi, yr=[-1,1], $
      xtit='Orbits', ytit='Residuals (!Uo!N)'
x0 = !x.crange[0]
dx = !x.crange[1]-!x.crange[0]
y0 = !y.crange[0]
dy = !y.crange[1]-!y.crange[0]
;oplot, x0+dx*[.5,.57], y0+dy*.915*[1,1]
xyouts, x0+dx*.4, y0+dy*.9, 'arctan(!Mf)-!Mf, using Tiscareno Eq. 16', $
        chars=1
oplot, Omegat/2/!pi, ft, co=ctred()
;oplot, x0+dx*[.5,.57], y0+dy*.865*[1,1], co=ctred()
xyouts, x0+dx*.4, y0+dy*.85, 'F(t) from Tiscareno Eq. 15', chars=1, co=ctred()
oplot, Omegat/2/!pi, (atan(tanphi)-atan(tanphi_schmidt))*180/!pi, co=ctgreen()
;oplot, x0+dx*[.5,.57], y0+dy*.815*[1,1], co=ctgreen()
xyouts, x0+dx*.4, y0+dy*.8, 'Tiscareno Eq. 16 - Schmidt Eq. 32', chars=1, $
        co=ctgreen()
if keyword_set(dolzr) then clzr
stop

window, 1
!p.multi = 0
for j=0,360 do begin
  plot, /xs, /ys, Omegat/2/!pi, atan((1-cos(Omegat))/$
                                     (1.5*Omegat-2*sin(Omegat)))*180/!pi, $
        yr=[-10,10], xtit='Orbits', $
        ytit='Cant Angle (!Uo!N) for!Cvarious initial velocity directions'
  phi0 = 2*!pi*j/360
  tanphi = ( -.5*sin(phi0)*sin(Omegat) + cos(phi0)*(1-cos(Omegat)) )/$
           ( -sin(phi0)*(1-cos(Omegat)) + cos(phi0)*(1.5*Omegat-2*sin(Omegat)) )
  oplot, Omegat/2/!pi, atan(tanphi)*180/!pi, co=ctred()
  oplot, Omegat/2/!pi, atan((1-cos(Omegat))/$
                            (1.5*Omegat-2*sin(Omegat)))*180/!pi
  wait, .1
endfor

end
