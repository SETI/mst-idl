if keyword_set(dolzr) then begin
  lzr, 'bookchapter_mst1'
  @plot_prepare
endif
restore, 'soirings_some_wave.13e5.sav'
!p.multi = [0,1,3]
!p.charsize = 3
plot, xiout, val, /xs, /ys, yticki=5, ytit='!MD!Ms(r)', xtit='!Mx'
if keyword_set(dolzr) then clzr

end
