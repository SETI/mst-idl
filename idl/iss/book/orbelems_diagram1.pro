if keyword_set(dolzr) then begin
  lzr, 'orbelems_diagram1'
  @plot_prepare
  plot_color
endif
device, decomposed=0

theta = findgen(361)*!pi/180

aa = 0.8
bb = 0.7
ee = sqrt( 1 - (bb/aa)^2 )
ff = 55*!dpi/180
rr = aa*(1-ee^2)/(1+ee*cos(ff))
plot, [-1,1], [-1,1], /nodata, xs=5, ys=5, /iso
oplot, [0,0], [-bb,bb], l=1
oplot, [-aa,-aa], [0,-bb/3*1.1], l=1
oplot, [aa*ee,aa*ee], [0,-bb/3*1.1], l=1
oplot, [-aa,aa], [0,0], l=1

oplot, aa*cos(theta), bb*sin(theta)
xyouts, aa*cos(120*!dpi/180)-.05, bb*sin(120*!dpi/180), 'orbit', $
        chars=1.5, align=1

foo = where( theta gt 9*!pi/5, count )
oplot, aa*ee+0.12*cos(theta[foo]), 0.12*sin(theta[foo])
arrow, aa*ee+0.12*cos(theta[foo[count-2]]), 0.12*sin(theta[foo[count-2]]), $
       aa*ee+0.12*cos(theta[foo[count-1]]), 0.12*sin(theta[foo[count-1]]), $
       hsize=!d.x_size/128, hthick=1, /solid, thick=2, /data
xyouts, aa*ee+0.14*cos(19*!pi/10), 0.14*sin(19*!pi/10)-0.02, '!Mv', chars=1.5
;oplot, aa*ee+0.2*cos(theta[foo]), 0.2*sin(theta[foo])  ; Starting lambda

foo = where( theta lt ff, count )
;oplot, aa*ee+0.2*cos(theta[foo]), 0.2*sin(theta[foo])
;arrow, aa*ee+0.2*cos(theta[foo[count-2]]), 0.2*sin(theta[foo[count-2]]), $
;       aa*ee+0.2*cos(theta[foo[count-1]]), 0.2*sin(theta[foo[count-1]]), $
;       hsize=!d.x_size/128, hthick=1, /solid, thick=2, /data
;xyouts, aa*ee+0.2*cos(ff/2), 0.2*sin(ff/2), '!Ml (if I << 1)'
oplot, aa*ee+0.1*cos(theta[foo]), 0.1*sin(theta[foo])
arrow, aa*ee+0.1*cos(theta[foo[count-2]]), 0.1*sin(theta[foo[count-2]]), $
       aa*ee+0.1*cos(theta[foo[count-1]]), 0.1*sin(theta[foo[count-1]]), $
       hsize=!d.x_size/128, hthick=1, /solid, thick=2, /data
xyouts, aa*ee+0.12*cos(ff/2), 0.12*sin(ff/2)-0.02, 'f', chars=1.5

arrow, aa*ee, 0, aa*ee+0.7*cos(!pi/5), -0.7*sin(!pi/5), hsize=!d.x_size/128, $
       hthick=1, /solid, thick=2, /data, co=gray()
xyouts, aa*ee+0.7*cos(!pi/5), -0.7*sin(!pi/5)-0.05, 'reference!Cdirection', chars=1.5, co=gray(), align=.5

polyfill, aa*ee+0.05*cos(theta), 0.05*sin(theta)
xyouts, aa*ee*0.9, 0.05, 'planet', chars=1.5, align=1

if !d.name eq 'X' then blank=0 else blank=white()
polyfill, -aa*ee+0.05*cos(theta), 0.05*sin(theta), co=blank
oplot, -aa*ee+0.05*cos(theta), 0.05*sin(theta), l=1
xyouts, -aa*ee, 0.14, 'empty!Cfocus', chars=1.5, align=.5

polyfill, aa+0.01*cos(theta), 0.01*sin(theta), co=blank
oplot, aa+0.01*cos(theta), 0.01*sin(theta)
xyouts, aa*1.03, 0, 'periapse', chars=1.5

polyfill, -aa+0.01*cos(theta), 0.01*sin(theta), co=blank
oplot, -aa+0.01*cos(theta), 0.01*sin(theta)
xyouts, -aa*1.03, 0, 'apoapse', chars=1.5, align=1

polyfill, aa*ee+rr*cos(ff)+0.01*cos(theta), rr*sin(ff)+0.01*sin(theta)
xyouts, aa*ee+rr*cos(ff), rr*sin(ff), ' moon or ring particle', chars=1.5

oplot, [aa*ee,aa*ee+rr*cos(ff)], [0,rr*sin(ff)]
xyouts, mean([aa*ee,aa*ee+rr*cos(ff)]), rr*sin(ff)/2+0.02, 'r', chars=1.5, align=1

xyouts, -aa/2, -bb/3*1.07, 'a', align=.5, chars=1.5
arrow, -aa/2*.93, -bb/3, 0, -bb/3, hsize=!d.x_size/128, hthick=1, $
       /solid, thick=2, /data
arrow, -aa/2*1.07, -bb/3, -aa, -bb/3, hsize=!d.x_size/128, hthick=1, $
       /solid, thick=2, /data
xyouts, aa*ee/2, -bb/3*1.07, 'ae', align=.5, chars=1.5
arrow, aa*ee/2*.78, -bb/3, 0, -bb/3, hsize=!d.x_size/128, hthick=1, $
       /solid, thick=2, /data
arrow, aa*ee/2*1.22, -bb/3, aa*ee, -bb/3, hsize=!d.x_size/128, hthick=1, $
       /solid, thick=2, /data

arrow, aa*ee, 0, aa*ee+0.3, 0, hsize=2*!d.x_size/128, hthick=3, $
       /solid, thick=6, /data
xyouts, aa*ee+0.3, -0.07, 'x', chars=1.5, align=.5
xyouts, aa*ee+0.3, -0.06, '^', chars=1.5, align=.5

arrow, aa*ee, 0, aa*ee, 0.3, hsize=2*!d.x_size/128, hthick=3, $
       /solid, thick=6, /data
xyouts, aa*ee-0.03, 0.3, 'y', chars=1.5, align=.5
xyouts, aa*ee-0.03, 0.31, '^', chars=1.5, align=.5

if keyword_set(dolzr) then clzr

end
