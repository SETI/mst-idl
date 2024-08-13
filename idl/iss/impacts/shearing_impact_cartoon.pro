notn = replicate(' ',20)
theta = findgen(361)*!pi/180
for _tt=0,15 do begin
  tt = _tt*3*!pi/10
  if keyword_set(dolzr) then begin
    lzr, 'shearing_impact_cartoon_'+string(_tt,fo='(I02)')
    @plot_prepare
    plot_color
    gray = ltgray()
  endif else gray = ctltgray()
  plot, [-16,16], [-2,2], /nodata, /iso, /xs, /ys, $
        xtickn=notn, ytickn=notn, xtickle=1e-10, ytickle=1e-10
  polyfill, cos(theta) - tt*sin(theta), sin(theta), co=gray
  oplot, cos(theta) - tt*sin(theta), sin(theta)
  xyouts, -15, -1.5, 't = '+string(tt/3/!pi,fo='(F3.1)')+' orbits', chars=1.5
  if keyword_set(dolzr) then clzr else wait, 0.1
endfor

end

