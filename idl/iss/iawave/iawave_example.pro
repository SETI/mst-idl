if keyword_set(dolzr) then begin
  lzr, 'iawave_example', /half
  @plot_prepare
endif
!p.thick = 10
!p.multi = [0,1,10]
rr = dindgen(1000)+100000
wave = fdensity_wave5( rr, a=1, xi_d=12, mm=3, rres=100000.0d0, $
                       sigma=6000, phi=0 )
plot, tkm(rr), wave, xs=5, ys=5
if keyword_set(dolzr) then clzr

end
