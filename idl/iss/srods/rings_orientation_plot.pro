restore, '/home/borogove/mmhedman/vimsocc/digest/gamCru100_dig_081009.sav'
sm=100;0
if keyword_set(dolzr) then begin
  psname = 'rings_orientation_plot'
  if keyword_set(plot_opacity) then psname = psname + '_opacity'
  lzr, psname
  @plot_prepare
endif
!p.multi = [0,1,3]
!p.charsize = 2
if keyword_set(oldplot) then begin
  plot, radii/1000, 390-smooth(data,sm), /xs, /ys, yr=[0,400], xr=[110,141], $
        xtit='Radius (1000 km)', ytit='Relative Optical Depth', $
        ytickn=replicate(' ',20), ytickle=1e-10
endif else if keyword_set(plot_opacity) then begin
  plot, radii/1000, $
        smooth(1-exp(alog((data>0<390)/390)*sin(62*!pi/180)),100,/nan), /xs, $
        /ys, yr=[0,1], xr=[100,141], xtit='Radius (1000 km)', ytit='Opacity'
endif else begin
  plot, radii/1000, smooth(-alog((data>0<390)/390)*sin(62*!pi/180),100,/nan), $
        /xs, /ys, xr=[100,141], xtit='Radius (1000 km)', ytit='Optical Depth'
endelse
if keyword_set(dolzr) then clzr

end
