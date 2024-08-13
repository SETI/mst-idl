!p.multi = [0,2,2]
!y.omargin = [4,2]
!y.margin = 0
radscan_keepwin = 1
radscan_xr = [ 118.825, 118.855 ]

restore, 'Atlas_5_4_ZetOri047.sav'
plot, tkm(rad_uvis_zetori047), tau_uvis_zetori047, /xs, /ys, xr=radscan_xr, $
      xtickn=replicate(' ',20), ytit='Normal !Mt'
xyouts, 118.8265, !y.crange[1] - (!y.crange[1]-!y.crange[0])*.1, $
        'UVIS (047 !Mz Ori)'

restore, 'RSS_Atlas5t4_extended.sav'
plot, tkm(radi), val, /xs, /ys, xr=radscan_xr, $
      xtickn=replicate(' ',20), ytit='Normal !Mt'
xyouts, 118.8265, !y.crange[1] - (!y.crange[1]-!y.crange[0])*.1, $
        'RSS (010 ingress)'

restore, 'alpsco29_vims_at54_112307.sav'
plot, tkm(radi), tau, /xs, /ys, xr=radscan_xr, $
      xtit='Radius'+tkmtit(), ytit='Normal !Mt'
xyouts, 118.8265, !y.crange[1] - (!y.crange[1]-!y.crange[0])*.1, $
        'VIMS (029 !Ma Sco)'

imdir = '/home/borogove/iss/images/'
restore, imdir+'SOI/SOISPTURN/N1467345208_2_cal.scan1'
_errbar = errbar / sqrt(radscan_np)
plot, tkm(radi), val, /nodata, /xs, /ys, ytit='I/F', xtit='Radius'+tkmtit(), $
      xr=radscan_xr
polyfill, [ tkm(radi), tkm(reverse(radi)) ], $
          [ val-_errbar, reverse(val+_errbar) ], noclip=0, color=ctgray()
oplot, tkm(radi), val
xyouts, 118.8265, !y.crange[1] - (!y.crange[1]-!y.crange[0])*.1, $
        'ISS (SOISPTURN)'


