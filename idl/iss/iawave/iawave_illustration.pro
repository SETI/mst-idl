if keyword_set(dolzr) then begin
  lzr, 'iawave_illustration'
  @plot_prepare
endif

theta = findgen(360*4+1) * !pi / 180
!p.multi = [0,1,3]
notn = replicate(' ',20)
plot, theta, sin(theta), xtickle=1e-10, ytickle=1e-10, xtickn=notn, ytickn=notn, /xs, /ys, yr=[-4,4]
oplot, 3*!pi + reverse(!y.crange), !y.crange, l=2
oplot, 4*!pi + reverse(!y.crange), !y.crange, l=2

if keyword_set(dolzr) then clzr

end

