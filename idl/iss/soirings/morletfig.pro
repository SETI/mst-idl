timeonly=1
@plot_prepare
if keyword_set(dolzr) then begin
  lzr, 'morletfig'
endif ;else !p.charsize=2

!p.multi=[0,2,2]
tt = findgen(200)/25
bb = 4
ss = 1
omega0 = 6
ii = complex(0,1)

envelope = !dpi^(-0.25) * exp(-(tt-bb)^2/2/ss^2)
morlet = !dpi^(-0.25) * exp(ii*omega0*(tt-bb)/ss) * exp(-(tt-bb)^2/2/ss^2)
plot, tt, real_part(morlet), xtickle=1e-10, ytickle=1e-10, /ys, yr=[-1.1,1.1], $
      xtickn=[' ',' ',' ',' ',' '], ytickn=['-1',' ','0',' ','1'], $
      ytit='!MY!Dr,s!N(r'')'
oplot, tt, imaginary(morlet), l=5
oplot, tt, envelope, l=1
oplot, tt, -envelope, l=1
arrow, bb,.85,bb+ss,.85, /data, hthick=2, thick=2, hsize=!d.x_size/64./1.5, $
       /solid
arrow, bb,.85,bb-ss,.85, /data, hthick=2, thick=2, hsize=!d.x_size/64./1.5, $
       /solid
xyouts, bb, .9, align=.5, 's'
arrow, 0,-.95,bb,-.95, /data, hthick=2, thick=2, hsize=!d.x_size/64./1.5, /solid
xyouts, 2, -.9, align=.5, 'r'

if keyword_set(timeonly) then goto, finish

omega = findgen(200)/20
omega_c = 4*!dpi/( omega0 + sqrt(2+omega0^2) )
morlet_fft = !dpi^(-0.25) * exp(-(ss*omega-omega0)^2/2)
; Value of the gaussian when the argument is +- sigma
gats = !dpi^(-0.25) * exp(-.5)
plot, omega, morlet_fft, xtickle=1e-10, ytickle=1e-10, $
      xtickn=[' ',' ',' ','!Mw!D0',' ',' '], ytickn=replicate(' ',20), $
      ytit='!S!MY!Dr,s!N(r'')!R!A^'
oplot, [omega0/ss,omega0/ss], [0,!dpi^(-0.25)], l=1
arrow, omega0/ss, gats, omega0/ss+1./ss, gats, /data, hthick=2, $
       thick=2, hsize=!d.x_size/64./1.5, /solid
arrow, omega0/ss, gats, omega0/ss-1./ss, gats, /data, hthick=2, $
       thick=2, hsize=!d.x_size/64./1.5, /solid
xyouts, omega0/ss, gats+.02, align=.5, '1/s'

finish:
if keyword_set(dolzr) then clzr

end
