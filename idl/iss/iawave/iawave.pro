; 112/MNRNGSHAD009 (0)
; 114/AZSCANLIT (0,40,60,150)
; 116/EQXSHADOW001 (35,38)
; 116/EQXSHADOW013 (63)
spawn, 'pwd', pwd
lastslash = rstrpos( pwd[0], '/' )
pwd = strmid( pwd[0], lastslash-3, 3 ) + $
      strlowcase(strmid( pwd, lastslash+1, 1000 ))
restore, 'stretch.sav'
if not keyword_set(jjj) then jjj = 0
image_name = filenames[jjj]
order1 = 1
noplot = 1
@caviar
noplot = 0
@restore_radscan1
wavelet_xr = [120.9>(min(radi)/1000),max(radi)/1000<125.2]
if pwd eq '112mnrngshad009' then wavelet_xr = [120.9,123.8]
equal = 0
minlam = 25
wavelet_wavelength = [ '30', '100', '300' ]
wavelet_plotyti = 0.002
specify_levels = 1
foo = where( ring_rads gt wavelet_xr[0]*1000 and $
             ring_rads lt wavelet_xr[1]*1000, count )
if count ne 7 then print, 'Warning'
ring_rads = ring_rads[foo]
ring_rads_legend = ring_rads_legend[foo]
if keyword_set(dolzr) then lzr, pwd+'_'+strtrim(jjj,2)+'_sigma' else window, 1
!p.multi = [0,2,2]
@iawave_sigma
if keyword_set(dolzr) then clzr
!p.multi = 0
if keyword_set(dolzr) then lzr, pwd+'_'+strtrim(jjj,2)+'_iawave' else window
if keyword_set(dolzr) then plot_color
hang = 1
@run_wavelet1
plot_wavemodel, ring_rads[0], ring_rads_legend[0], sigma, tkmradi=tkm(radi)
oplot, ring_rads[[3,3]]/1000, !y.crange, l=1
if keyword_set(dolzr) then clzr
if keyword_set(dolzr) then lzr, pwd+'_'+strtrim(jjj,2)+'_iawave2' else window, 2
wavelet_xr[1] = a1/1000 + 0.3
!x.omargin = [0,10]
@run_wavelet1
plot_wavemodel, ring_rads[0], ring_rads_legend[0], sigma, tkmradi=tkm(radi)
plot_wavemodel, ring_rads[1], ring_rads_legend[1], sigma[cassdiv], tkmradi=tkm(radi[cassdiv])
oplot, ring_rads[[3,3]]/1000, !y.crange, l=1
!x.omargin = 0
if keyword_set(dolzr) then clzr
