rr1 = [ 0.1, 3.3, 32 ]
line1 = [ 3.2e-3, 1e-4, 1e-5 ]
rr2 = [ 0.1, 2.7, 28 ]
line2 = [ 2.7e-3, 1e-4, 1e-5 ]

rr = 10^( findgen(41)/10 - 1 )
tau1 = 3.33e-4 / rr
tau2 = 2.75e-4 / rr

if keyword_set(dolzr) then begin
  lzr, 'rhearpx_grainsize', /half
  @plot_prepare
endif
!p.multi = [0,2,2]
plot, rr, tau1, /xs, /ys, /xlog, /ylog, xtit='Grain size (cm)', ytit='Edge-on optical depth'
oplot, rr, tau2, l=3
oplot, 10^!x.crange, [1,1]*1.5e-6, l=1
if keyword_set(dolzr) then clzr

end
