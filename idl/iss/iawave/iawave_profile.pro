restore, '$DATA/images/116/EQXSHADOW001/N1628594653_1_cal.scan2'

if keyword_set(dolzr) then begin
  lzr, 'iawave_profile', /half
  @plot_prepare
endif
!p.multi = [0,1,3]
!p.charsize = 1.5
plot_nosci, tkm(radi), val, /xs, /ys, xr=[120.9,125.2], $
      xtit='Radius'+tkmtit(), ytit='I/F'
if keyword_set(dolzr) then clzr

end

