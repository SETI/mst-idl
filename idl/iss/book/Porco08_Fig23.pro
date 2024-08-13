restore, 'vs_by_sizes.dat'
restore, 'bridges_vs.dat'

if keyword_set(dolzr) then begin
  lzr, 'Porco08_Fig23_mod', /port
  @plot_prepare
  plot_color
  device, /cmyk
endif else window, xs=709, ys=1280

!p.multi = [0,1,6]
!p.charsize = 3;1.5
!y.margin = 0
!y.omargin = [4,2]
plot, [0,.8], [0,3300], /nodata, /xs, /ys, $
      xtickn=replicate(' ',20), ytit='# Particles'
hist = histogram( v1[large1], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10, co=ctyellow();, l=2
hist = histogram( v1[medium1], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10, co=ctcyan();, l=1
hist = histogram( v1[small1], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10, co=ctpurple()
xyouts, 0.5, 2500, 'Supulver et al. (1995)', chars=1.5, co=ctred()
;oplot, [0.15,0.19], 2750*[1,1]
;oplot, [0.15,0.19], 2400*[1,1], l=1
;oplot, [0.15,0.19], 2050*[1,1], l=2
xyouts, 0.2, 2650, 'Small ( < 50 cm )', chars=1, co=ctpurple()
xyouts, 0.2, 2300, 'Intermediate', chars=1, co=ctcyan()
xyouts, 0.2, 1950, 'Large ( > 1 m )', chars=1, co=ctyellow()
plot, [0,.8], [0,3300], /nodata, /xs, /ys, $
      xtickn=replicate(' ',20), ytit='# Particles'
hist = histogram( v4[large4], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10, co=ctyellow();, l=2
hist = histogram( v4[medium4], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10, co=ctcyan();, l=1
hist = histogram( v4[small4], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10, co=ctpurple()
xyouts, 0.5, 2500, 'Bridges et al. (1984)', chars=1.5, co=ctblue()
plot, [0,.8], [0,3300], /nodata, /xs, /ys, $
      xtit='Relative Velocity (cm/sec)', ytit='# Particles'
hist = histogram( v2[large2], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10, co=ctyellow();, l=2
hist = histogram( v2[medium2], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10, co=ctcyan();, l=1
hist = histogram( v2[small2], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10, co=ctpurple()
xyouts, 0.5, 2500, 'Borderies et al. (1984)!C     v*=0.001 cm/sec', chars=1.5, $
        co=ctgreen()

if keyword_set(dolzr) then clzr

end
