restore, 'ptle_v_and_size.dat'

if keyword_set(dolzr) then begin
  lzr, 'porco08_figs1', /port
  @plot_prepare
endif else window, xs=709, ys=1280

!p.multi = [0,1,6]
!p.charsize = 1.5
!y.margin = 0
!y.omargin = [4,2]
hist = histogram( v1[large1], min=0, max=.8, nbins=81, locations=locations )
plot, [0,locations+.005], hist, ps=10, xr=[0,.8], yr=[0,3300], /ys, l=2, $
      xtickn=replicate(' ',20), ytit='Numbers of Particles'
hist = histogram( v1[medium1], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10, l=1
hist = histogram( v1[small1], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10
xyouts, 0.5, 2500, 'Supulver'
hist = histogram( v2[large2], min=0, max=.8, nbins=81, locations=locations )
plot, [0,locations+.005], hist, ps=10, xr=[0,.8], yr=[0,3300], /ys, l=2, $
      xtit='Relative Velocity (cm/sec)', ytit='Numbers of Particles'
hist = histogram( v2[medium2], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10, l=1
hist = histogram( v2[small2], min=0, max=.8, nbins=81, locations=locations )
oplot, [0,locations+.005], hist, ps=10
xyouts, 0.5, 2500, 'Borderies!Cv*=0.01 cm/sec'

if keyword_set(dolzr) then clzr

end
