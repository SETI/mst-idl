if keyword_set(dolzr) then lzr, image_name+'_wavelet_Mi42bw'
if keyword_set(dolzr) then plot_color
if keyword_set(dolzr) then specify_levels=1 else specify_levels=0
if keyword_set(dolzr) then !p.thick=2
if keyword_set(dolzr) then !x.thick=2
if keyword_set(dolzr) then !y.thick=2
if keyword_set(dolzr) then !p.font=1
if keyword_set(dolzr) then spawn, 'pwd', pwd
if keyword_set(dolzr) then lastslash2 = rstrpos( pwd[0], '/' )
if keyword_set(dolzr) then lastslash2 = rstrpos( pwd[0], '/', lastslash2-1 )
wavelet_tit = strmid(image_name,0,11)+' ('+strmid(pwd[0],lastslash2+1,1000)+')'
if keyword_set(!x.crange) then ring_rads_legend = ring_rads_legend[where( $
   ring_rads gt !x.crange[0]*1000 and ring_rads lt !x.crange[1]*1000 )]
if keyword_set(!x.crange) then ring_rads = ring_rads[where( $
   ring_rads gt !x.crange[0]*1000 and ring_rads lt !x.crange[1]*1000 )]
equal=0
mooncolor=1
@run_wavelet1
foo = where( ring_rads_legend eq 'Pr 4:3' or $
             ring_rads_legend eq 'Ja 3:2' or $
             ring_rads_legend eq 'Mi 4:2 BW' or $
             ring_rads_legend eq 'Mi 4:2' )
plot_wavemodel, ring_rads[foo], ring_rads_legend[foo], 75, /mooncolor
plot_wavemodel, ring_rads[foo], ring_rads_legend[foo], 150, /mooncolor
if keyword_set(dolzr) then clzr
