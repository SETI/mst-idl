restore, '$DATA/images/vims/089/GAMCRU/VIMS_2008_290_GAMCRU_I_TAU_01KM.sav'

if keyword_set(dolzr) then begin
  lzr, 'iawave_context', /half
  @plot_prepare
endif
!p.multi = [0,1,3]
!p.charsize = 1.5
plot, tkm(radius), smooth(tau,4), /xs, /ys, xr=[120.9,125.2], yr=[0,2.2], $
      xtit='Radius'+tkmtit(), ytit='Optical Depth !Mt'
xyouts, 122.05, 2, 'A Ring!CInner Edge', align=.5, chars=0.8
oplot, [122.05,122.05], [.8,1.7], l=1
xyouts, 122.313-.05, 0.1, 'Pandora 5:4 DW', chars=0.8
oplot, [122.313,122.313], [.3,.7], l=1
xyouts, mean([!x.crange[0],122.05]), 1.4, 'Cassini Division Ramp', $
        align=.5, chars=0.8
arrow, 121.82, 1.45, 122.05, 1.45, hsize=!d.x_size/128, hthick=1, $
       /solid, thick=2, /data
arrow, 121.13, 1.45, !x.crange[0], 1.45, hsize=!d.x_size/128, hthick=1, $
       /solid, thick=2, /data
xyouts, mean([122.313,!x.crange[1]]), .4, 'Inner A Ring Chaotic Region', $
        align=.5, chars=0.8
arrow, 124.19, .45, !x.crange[1], .45, hsize=!d.x_size/128, hthick=1, $
       /solid, thick=2, /data
arrow, 123.32, .45, 122.313, .45, hsize=!d.x_size/128, hthick=1, $
       /solid, thick=2, /data
if keyword_set(dolzr) then clzr

end

