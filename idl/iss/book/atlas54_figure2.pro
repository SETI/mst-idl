!p.charsize = 1.5
get_color
radscan_xr = [ 118.82, 118.86 ]*1000

plot_nosci, radscan_xr, [0,.3], /nodata, /xs, ys=9, xma=[10,10], $
            xtit='Ring Plane Radius (km)', ytit='Normal Optical Depth'

restore, 'Atlas_5_4_ZetOri047.sav'
oplot, rad_uvis_zetori047, tau_uvis_zetori047, co=blue()
oplot, [118849.5,118854], [.235,.235], co=blue()
xyouts, 118854.5, 0.23, 'UVIS'

restore, 'RSS_Atlas5t4_extended.sav'
oplot, radi, val, co=red()
oplot, [118849.5,118854], [.275,.275], co=red()
xyouts, 118854.5, 0.27, 'RSS'

restore, 'alpsco29_vims_at54_112307.sav'
oplot, radi, tau, co=green()
oplot, [118849.5,118854], [.255,.255], co=green()
xyouts, 118854.5, 0.25, 'VIMS'

imdir = '/home/borogove/iss/images/'
restore, imdir+'SOI/SOISPTURN/N1467345208_2_cal.scan1'
_errbar = errbar / sqrt(radscan_np)  ; Convert to standard error of the mean
oplot, [118823.5,118828], [.265,.265]
xyouts, 118828.5, 0.26, 'ISS'
axis, yaxis=1, yr=[0,.015], /data, /save, ytit='I/F'
polyfill, [ radi, reverse(radi) ], $
          [ val-_errbar, reverse(val+_errbar) ], noclip=0, color=gray()
oplot, radi, val


