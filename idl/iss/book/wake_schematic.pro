nx = 2000
j1 = 20.
j2 = 50.
dj = 2.5
if keyword_set(dolzr) then begin
  lzr, 'wake_schematic'
  @plot_prepare
  plot_color
endif
!p.multi = [0,1,3]
!p.charsize = 2
plot, [-nx,nx], (j2+dj)*[-1,1], /xs, /ys, /nodata, xtit='Azimuthal Distance (km)', ytit='Radial Distance (km)'
polyfill, 35*cos(findgen(361)*!dpi/180), $
          5*sin(findgen(361)*!dpi/180)

xx = findgen(nx)
damp = .001
aa = 136505.0d0       ; Daphnis semimajor axis
mm = 8d13 / 5.68d26   ; Daphnis mass, as a fraction of Saturn's
for j=j1,j2,2 do begin
  lambda = 3*!dpi*j
  ee = 4./3*mm*aa^2/j^2
  amp = 2*aa*ee*exp(-xx*damp)
  oplot, [!x.crange[0],0], [-j,-j]
  oplot, xx, -j + amp*sin(2*!dpi*xx/lambda)
  oplot, -xx, j - amp*sin(2*!dpi*xx/lambda)
  oplot, [0,!x.crange[1]], [j,j]
endfor
polyfill, -1*[1400,1150,1150,1000,1150,1150,1400,1400], $
          -1*[40,40,43,35,27,30,30,40], co=ctwhite()
oplot, -1*[1400,1150,1150,1000,1150,1150,1400,1400], $
       -1*[40,40,43,35,27,30,30,40], thick=5
polyfill, -1*[1425,1675,1675,1425,1425], -1*[45,45,25,25,45], co=ctwhite()
oplot, -1*[1425,1675,1675,1425,1425], -1*[45,45,25,25,45], thick=3
xyouts, -1550, -33, 'Relative!CMotion', align=.5, chars=1
polyfill, [1400,1150,1150,1000,1150,1150,1400,1400], $
          [40,40,43,35,27,30,30,40], co=ctwhite()
oplot, [1400,1150,1150,1000,1150,1150,1400,1400], $
       [40,40,43,35,27,30,30,40], thick=5
polyfill, [1425,1675,1675,1425,1425], [45,45,25,25,45], co=ctwhite()
oplot, [1425,1675,1675,1425,1425], [45,45,25,25,45], thick=3
xyouts, 1550, 37, 'Relative!CMotion', align=.5, chars=1

arrow, 100, 46, 200, 46, hsize=2*!d.x_size/128, hthick=2, /solid, thick=10, color=ctred(), /data
arrow, 100, 23, 250, 23, hsize=2*!d.x_size/128, hthick=2, /solid, thick=10, color=ctred(), /data
arrow, 100, 0, 300, 0, hsize=2*!d.x_size/128, hthick=2, /solid, thick=10, color=ctred(), /data
arrow, 100, -23, 350, -23, hsize=2*!d.x_size/128, hthick=2, /solid, thick=10, color=ctred(), /data
arrow, 100, -46, 400, -46, hsize=2*!d.x_size/128, hthick=2, /solid, thick=10, color=ctred(), /data
xyouts, 350, -3, 'Absolute Motion', color=ctred(), chars=1

if keyword_set(dolzr) then clzr

end
