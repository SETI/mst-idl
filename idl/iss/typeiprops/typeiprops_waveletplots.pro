cd, '$DATA/iss/images/031/RDHRCOMP'
restore, 'stretch.sav'
image_name = filenames[50]
@caviar
radi = radi + 9
equal = 0
minlam = 2
@run_wavelet1
plot_wavemodel, ring_rads, ring_rads_legend, 20, /mooncolor
radscan_xr = [134.87,134.94]
.run plot_radscan

cd, '$DATA/iss/images/032/RDHRCOMP'
restore, 'stretch.sav'
image_name = filenames[50]
@caviar
radi = radi - 4
equal = 0
minlam = 2
@run_wavelet1
plot_wavemodel, ring_rads, ring_rads_legend, 20, /mooncolor
radscan_xr = [134.87,134.94]
.run plot_radscan

cd, '$DATA/iss/images/046/RDHRESSCN'
restore, 'stretch.sav'
image_name = filenames[3]
@caviar
radi = radi + 1
equal = 0
minlam = 2
@run_wavelet1
plot_wavemodel, ring_rads, ring_rads_legend, 20, /mooncolor
radscan_xr = [134.87,134.94]
.run plot_radscan

cd, '$DATA/iss/images/077/RDHRCOMP'
restore, 'stretch.sav'
image_name = filenames[6]
@caviar
equal = 0
minlam = 2
@run_wavelet1
plot_wavemodel, ring_rads, ring_rads_legend, 20, /mooncolor
radscan_xr = [134.87,134.94]
.run plot_radscan

cd, '$DATA/iss/images/132/PROPELLR'
restore, 'stretch.sav'
image_name = filenames[8]
@caviar
radi = radi - 4
equal = 0
minlam = 2
@run_wavelet1
plot_wavemodel, ring_rads, ring_rads_legend, 20, /mooncolor
radscan_xr = [134.87,134.94]
.run plot_radscan

; The resulting radi and val have been transferred to files 
; 077rdhrcomp.sav and 132propellr.sav in ~/idl/iss/typeiprops
cd, '~/idl/iss/typeiprops'
restore, '031rdhrcomp.sav'
restore, '032rdhrcomp.sav'
restore, '046rdhresscn.sav'
restore, '077rdhrcomp.sav'
restore, '132propellr.sav'
device, decomposed=0
if keyword_set(dolzr) then begin &$
  lzr, 'typeiprops_data', /half &$
  @plot_prepare &$
  plot_color &$
  device, /cmyk &$
  !p.multi = [0,1,2] &$
endif
plot_nosci, [134897,134927], [0.96,1.04], xs=5, /ys, /nodata, $
      ytit='Relative I/F', xtick_get=xtg
polyfill, 134912+[2,4,4,2,2], !y.crange[[0,0,1,1,0]], co=ltgray()
polyfill, 134912-[2,4,4,2,2], !y.crange[[0,0,1,1,0]], co=ltgray()
axis, xaxis=0, /xs, xtit='Radius (km)', xtickn=string(xtg,'(I6)')
axis, xaxis=1, /xs, xtickn=replicate(' ',20)
oplot, radi_132propellr008, val_132propellr008/mean(val_132propellr008)
oplot, radi_077rdhrcomp006, val_077rdhrcomp006/mean(val_077rdhrcomp006), $
       co=red()
oplot, radi_031rdhrcomp050, val_031rdhrcomp050/mean(val_031rdhrcomp050), $
       co=green()
oplot, radi_032rdhrcomp050, val_032rdhrcomp050/mean(val_032rdhrcomp050), $
       co=cyan()
oplot, radi_046rdhresscn003, $
       val_046rdhresscn003/mean(val_046rdhresscn003), co=blue()
oplot, [1,1]*134912, !y.crange, l=1
if keyword_set(dolzr) then clzr
