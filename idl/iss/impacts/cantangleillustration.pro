if keyword_set(dolzr) then begin
  lzr, 'cantangleillustration', /half
  @plot_prepare
endif

notn = replicate(' ',20)
plot, [-5,5], [0,0], /xs, /ys, yr=[-.75,.75], l=1, /iso, $
      xtickn=notn, ytickn=notn, xtickle=1e-10, ytickle=1e-10
oplot, [-4,4], [.5,-.5], thick=5
xyouts, -.05, -1.0, /noclip, 'Longitude', align=1
arrow, 0, -.94, 1, -.94, hsize=!d.x_size/128, hthick=1, /solid, $
       thick=2, /data, /noclip
xyouts, -5.07, -.1, /noclip, 'Radius', align=1, orient=90
arrow, -5.12, -.05, -5.12, .7, hsize=!d.x_size/128, hthick=1, /solid, $
       thick=2, /data, /noclip
theta = atan(1./8)*180/!pi
thetai = findgen(100)/99*theta
xyouts, 3.4, -.38, 'Sheared Cloud', orient=-theta, align=1
oplot, replicate(2,100), !pi-thetai*!pi/180, /polar
arrowpolar, 2, !pi-thetai[98]*!pi/180, 2, !pi-thetai[99]*!pi/180, $
            hsize=!d.x_size/128, hthick=1, /solid, thick=2, /data
xyouts, -2.1, .06, 'Cant Angle !Mq', align=1

xyouts, -4.8, .5, '(b)'

if keyword_set(dolzr) then clzr

end
