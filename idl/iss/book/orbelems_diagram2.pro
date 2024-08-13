if keyword_set(dolzr) then begin
  lzr, 'orbelems_diagram2'
  @plot_prepare
  plot_color
endif
device, decomposed=0

theta = findgen(361)*!pi/180 - !pi

;surface, dist(10), /nodata, /save, az=75, xs=5, ys=5, zs=5, $
;         xr=[1,-1], yr=[1,-3], zr=[-1,2]
;
;plots, [0,1], [0,0], [0,0], /t3d, color=gray()
;plots, [0,0], [0,1], [0,0], /t3d, color=gray()
;plots, [0,0], [0,0], [0,1], /t3d, color=gray()
;
;foo = where( theta gt -!pi/6 and theta lt 2*!pi/3 )
;plots, 0.6*cos(theta[foo]), 0.6*sin(theta[foo]), replicate(0,361),
;       /t3d, color=gray()
;;reference plane
;
;plots, [0,0.6*cos(!pi/4)], [0,0.6*sin(!pi/4)], [0,0], /t3d, l=1
;; line of nodes

plot, [-0.9,1.2], [-0.9,1.2], /nodata, /iso, $
;  xs=1, ys=1
  xs=5, ys=5

coords2d = plot3d_mst( [[0.,1],[0,0],[0,0]] )
;oplot, coords2d[*,0], coords2d[*,1], co=gray()
arrow, coords2d[0,0], coords2d[0,1], coords2d[1,0], coords2d[1,1], $
       hsize=!d.x_size/128, hthick=1, /solid, thick=2, /data, co=gray()
xyouts, coords2d[1,0], coords2d[1,1]-0.04, 'reference!Cdirection', chars=1, $
        co=gray(), align=.5
coords2d = plot3d_mst( [[0.,0],[0,1],[0,0]] )
arrow, coords2d[0,0], coords2d[0,1], coords2d[1,0], coords2d[1,1], $
       hsize=!d.x_size/128, hthick=1, /solid, thick=2, /data, co=gray()
coords2d = plot3d_mst( [[0.,0],[0,0],[0,0.5]] )
arrow, coords2d[0,0], coords2d[0,1], coords2d[1,0], coords2d[1,1], $
       hsize=!d.x_size/128, hthick=1, /solid, thick=2, /data, co=gray()

foo = where( theta gt -!pi/8 and theta lt 2*!pi/3, count )
coords2d = plot3d_mst([ [0.8*cos(theta[foo])], [0.8*sin(theta[foo])], $
                        [replicate(0,count)] ])
oplot, coords2d[*,0], coords2d[*,1], co=gray()
xyouts, 0.8, -0.12, 'reference!Cplane', chars=1, co=gray(), align=.5

ii = 30*!pi/180
cap_omega = 35*!pi/180
omega = 15*!pi/180
matrix = [[ cos(omega)*cos(cap_omega) - sin(omega)*cos(ii)*sin(cap_omega), $
            -sin(omega)*cos(cap_omega) - cos(omega)*cos(ii)*sin(cap_omega), $
            sin(ii)*sin(cap_omega) ], $
          [ cos(omega)*sin(cap_omega) + sin(omega)*cos(ii)*cos(cap_omega), $
            -sin(omega)*sin(cap_omega) + cos(omega)*cos(ii)*cos(cap_omega), $
            -sin(ii)*cos(cap_omega) ], $
          [ sin(omega)*sin(ii), cos(omega)*sin(ii), cos(ii) ]]

foo = where( theta gt -!pi/6 and theta lt !pi/7, count )
orbit = [ [cos(theta[foo])], [sin(theta[foo])], [replicate(0,count)] ]
coords2d = plot3d_mst(0.8*matrix ## orbit)
oplot, coords2d[*,0], coords2d[*,1]
xyouts, coords2d[count*2/3,0], coords2d[count*2/3,1], '   orbit', chars=1

coords2d = plot3d_mst([ [0,0.8*cos(cap_omega)], [0,0.8*sin(cap_omega)], [0,0] ])
oplot, coords2d[*,0], coords2d[*,1], l=1
if !d.name eq 'X' then blank=0 else blank=white()
polyfill, coords2d[1,0]+0.01*cos(theta), coords2d[1,1]+0.01*sin(theta), co=blank
oplot, coords2d[1,0]+0.01*cos(theta), coords2d[1,1]+0.01*sin(theta)
xyouts, coords2d[1,0], coords2d[1,1], '!Cascending!Cnode'

foo = where( theta gt 0 and theta lt cap_omega, count )
coords2d = plot3d_mst([ [0.3*cos(theta[foo])], [0.3*sin(theta[foo])], $
                        [replicate(0,count)] ])
oplot, coords2d[*,0], coords2d[*,1]
arrow, coords2d[count-2,0], coords2d[count-2,1], $
       coords2d[count-1,0], coords2d[count-1,1], $
       hsize=!d.x_size/128, hthick=1, /solid, thick=2, /data
xyouts, mean(coords2d[*,0]), mean(coords2d[*,1])-0.05, '!MW', align=.5

coords2d = plot3d_mst(0.8*matrix ## [[0,1],[0,0],[0,0]])
oplot, coords2d[*,0], coords2d[*,1], l=1
polyfill, coords2d[1,0]+0.01*cos(theta), coords2d[1,1]+0.01*sin(theta), co=blank
oplot, coords2d[1,0]+0.01*cos(theta), coords2d[1,1]+0.01*sin(theta)
xyouts, coords2d[1,0]+0.02, coords2d[1,1]-0.03, 'periapse'

foo = where( theta gt -omega and theta lt 0, count )
orbit = [ [cos(theta[foo])], [sin(theta[foo])], [replicate(0,count)] ]
coords2d = plot3d_mst(0.4*matrix ## orbit)
oplot, coords2d[*,0], coords2d[*,1]
arrow, coords2d[count-2,0], coords2d[count-2,1], $
       coords2d[count-1,0], coords2d[count-1,1], $
       hsize=!d.x_size/128, hthick=1, /solid, thick=2, /data
xyouts, mean(coords2d[*,0])+0.02, mean(coords2d[*,1])-0.03, '!Mw', align=.5

niarc = 31
iarc = [ [cos(omega/2)], [-sin(omega/2)], [0] ]*0.8
iarc = rebin( iarc, niarc, 3 )
for j=0,niarc-1 do begin
  i = ii*j/(niarc-1)
  matrix1 = [[ cos(omega)*cos(cap_omega) - sin(omega)*cos(i)*sin(cap_omega), $
               -sin(omega)*cos(cap_omega) - cos(omega)*cos(i)*sin(cap_omega), $
               sin(i)*sin(cap_omega) ], $
             [ cos(omega)*sin(cap_omega) + sin(omega)*cos(i)*cos(cap_omega), $
               -sin(omega)*sin(cap_omega) + cos(omega)*cos(i)*cos(cap_omega), $
               -sin(i)*cos(cap_omega) ], $
             [ sin(omega)*sin(i), cos(omega)*sin(i), cos(i) ]]
  iarc[j,*] = matrix1 ## iarc[j,*]
endfor
coords2d = plot3d_mst(iarc)
oplot, coords2d[*,0], coords2d[*,1]
arrow, coords2d[niarc-2,0], coords2d[niarc-2,1], $
       coords2d[niarc-1,0], coords2d[niarc-1,1], $
       hsize=!d.x_size/128, hthick=1, /solid, thick=2, /data
xyouts, mean(coords2d[*,0])+0.03, mean(coords2d[*,1]), 'I', align=.5

coords2d = plot3d_mst(0.3*matrix ## [[0,1],[0,0],[0,0]])
arrow, coords2d[0,0], coords2d[0,1], coords2d[1,0], coords2d[1,1], $
       hsize=2*!d.x_size/128, hthick=3, /solid, thick=6, /data
xyouts, coords2d[1,0]+0.01, coords2d[1,1]+0.01, 'x', chars=1, align=.5
xyouts, coords2d[1,0]+0.01, coords2d[1,1]+0.02, '^', chars=1, align=.5
coords2d = plot3d_mst(0.2*matrix ## [[0,0],[0,1],[0,0]])
arrow, coords2d[0,0], coords2d[0,1], coords2d[1,0], coords2d[1,1], $
       hsize=2*!d.x_size/128, hthick=3, /solid, thick=6, /data
xyouts, coords2d[1,0]+0.01, coords2d[1,1]+0.01, 'y', chars=1, align=.5
xyouts, coords2d[1,0]+0.01, coords2d[1,1]+0.02, '^', chars=1, align=.5
coords2d = plot3d_mst(0.2*matrix ## [[0,0],[0,0],[0,1]])
arrow, coords2d[0,0], coords2d[0,1], coords2d[1,0], coords2d[1,1], $
       hsize=2*!d.x_size/128, hthick=3, /solid, thick=6, /data
xyouts, coords2d[1,0], coords2d[1,1]+0.01, 'z', chars=1, align=.5
xyouts, coords2d[1,0], coords2d[1,1]+0.02, '^', chars=1, align=.5

if keyword_set(dolzr) then clzr

end
