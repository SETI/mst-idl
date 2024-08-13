restore, '$DATA/images/046/RDHRESSCN/N1560311433_1_cal.scan9'
restore, '$DATA/caviar/developmental/ring_rads.sav'
foo = where( _ring_rads gt min(radi) and $
             _ring_rads lt max(radi) and $
             strmid(_ring_rads_legend,3,4) ne '11:8' and $
             strmid(_ring_rads_legend,3,5) ne '15:11' )
foo = where( _ring_rads_legend eq 'Pr 6:5' )
ring_rads = _ring_rads[foo]
ring_rads_legend = _ring_rads_legend[foo]

wavelet_xr=[123.5,123.8]
hang = 1
minlam = 1.8
wavelet_plotyti = .01
mooncolor = 0
radi = radi+12
psname='046rdhresscn_pr65'
.run preparewaveletplots
@run_wavelet1
plot_wavemodel, ring_rads, ring_rads_legend, 40
if keyword_set(dolzr) then clzr
