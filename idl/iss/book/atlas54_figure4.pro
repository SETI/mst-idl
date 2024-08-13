get_color
!p.multi = [0,1,4]
!p.charsize = 2
!y.omargin = [4,2]
!y.margin = 0
radscan_keepwin = 1
radscan_xr = [ 118825.0d0, 118855 ]
px = 118826.0d0
py = .15

imdir = '/home/borogove/iss/images/'
restore, imdir+'SOI/SOISPTURN/N1467345208_2_cal.scan1'
_errbar = errbar / sqrt(radscan_np)
plot, radi, val, /nodata, /xs, /ys, ytit='I/F', xr=radscan_xr, $
      xtickn=replicate(' ',20), yticki=.002, yr=[.009,.0145], xtick_get=xtg
polyfill, [ radi, reverse(radi) ], $
          [ val-_errbar, reverse(val+_errbar) ], noclip=0, color=gray()
oplot, radi, val
xyouts, px, !y.crange[1] - (!y.crange[1]-!y.crange[0])*py, charsize=1, $
        'ISS';'ISS (SOISPTURN)'

restore, 'RSS_Atlas5t4_extended.sav'
plot, radi, val, /xs, /ys, xr=radscan_xr, yr=[.09,.28], $
      xtickn=replicate(' ',20), ytit='Normal!COptical Depth'
xyouts, px, !y.crange[1] - (!y.crange[1]-!y.crange[0])*py, charsize=1, $
        'RSS';'RSS (010 ingress)'

restore, 'Atlas_5_4_ZetOri047.sav'
plot, rad_uvis_zetori047, tau_uvis_zetori047, /xs, /ys, xr=radscan_xr, $
      yr=[.045,.13], xtickn=replicate(' ',20), $
      ytit='Normal!COptical Depth'
xyouts, px, !y.crange[1] - (!y.crange[1]-!y.crange[0])*py, charsize=1, $
        'UVIS';'UVIS (047 !Mz Ori)'

restore, 'alpsco29_vims_at54_112307.sav'
plot, radi, tau, /xs, /ys, xr=radscan_xr, yr=[.04,.2], yticki=.05, $
      xtickn=replicate(' ',20), ytit='Normal!COptical Depth'
xyouts, px, !y.crange[1] - (!y.crange[1]-!y.crange[0])*py, charsize=1, $
        'VIMS';'VIMS (029 !Ma Sco)'

axis, xaxis=0, /data, /xs, xtit='Ring Plane Radius (km)', $
      xtickn = string( xtg, fo='(I6)' )


