!p.charsize = 1.5
get_color
radscan_xr = [ 118.82, 118.86 ]*1000

xma = [10,10]
plot, radscan_xr, [-.05,.35], /nodata, /xs, ys=9, xma=xma, $
      ytit='Normal Optical Depth', $ ;xtit='Ring Plane Radius (km)', 
      xtickn=replicate(' ',20), xtick_get=xtg, $
      yticki=.05, ytickle=1e-10, ytickn=[' ',' ','','','','','',' ',' ']
axis, xaxis=0, /data, /xs, xtit='Ring Plane Radius (km)', $
      xtickn = string( xtg, fo='(I6)' )
for j=.04,.27,.01 do oplot, $
   !x.crange[0] + [ 0, (!x.crange[1]-!x.crange[0])*.01 ], [j,j]
for j=.05,.25,.05 do oplot, $
   !x.crange[0] + [ 0, (!x.crange[1]-!x.crange[0])*.02 ], [j,j]
for j=-.05,.08,.01 do oplot, $
   !x.crange[1] - [ 0, (!x.crange[1]-!x.crange[0])*.01 ], [j,j]
for j=-.05,.05,.05 do oplot, $
   !x.crange[1] - [ 0, (!x.crange[1]-!x.crange[0])*.02 ], [j,j]
if !d.name eq 'X' then xtl = .068 else xtl = .145
xyouts, !x.crange[1] + (!x.crange[1]-!x.crange[0])*xtl, $
        mean([-.05,.07]), $
        orient=90, align=.5, 'Normal Optical Depth'
oplot, [118821.5,118825.5], [.33,.33]
xyouts, 118826, 0.325, 'ISS'
oplot, [118821.5,118825.5], [.31,.31], co=red()
xyouts, 118826, 0.305, 'RSS'
oplot, [118850,118854], [.33,.33], co=blue()
xyouts, 118854.5, 0.325, 'UVIS'
oplot, [118850,118854], [.31,.31], co=green()
xyouts, 118854.5, 0.305, 'VIMS'

restore, 'Atlas_5_4_ZetOri047.sav'
oplot, rad_uvis_zetori047, tau_uvis_zetori047, co=blue()

restore, 'RSS_Atlas5t4_extended.sav'
oplot, radi, val, co=red()

restore, 'alpsco29_vims_at54_112307.sav'
oplot, radi, tau-.1, co=green()

imdir = '/home/borogove/iss/images/'
restore, imdir+'SOI/SOISPTURN/N1467345208_2_cal.scan1'
_errbar = errbar / sqrt(radscan_np)  ; Convert to standard error of the mean
axis, yaxis=1, yr=!y.crange+.1, /data, /ys, $
      yticki=.05, ytickle=1e-10, ytickn=['','','',' ',' ',' ',' ',' ',' ']
axis, yaxis=1, yr=[-.005,.0145], /data, /ys, /save, $
      yticki=.002, ytickle=1e-10, ytickn=[' ',' ',' ',' ',' ',' ',' ','','','']
for j=.009,.014,.0005 do oplot, $
   !x.crange[1] - [ 0, (!x.crange[1]-!x.crange[0])*.01 ], [j,j]
for j=.01,.014,.002 do oplot, $
   !x.crange[1] - [ 0, (!x.crange[1]-!x.crange[0])*.02 ], [j,j]
if !d.name eq 'X' then xtl = .08 else xtl = .17
xyouts, !x.crange[1] + (!x.crange[1]-!x.crange[0])*xtl, $
        mean([.01,.014]), $
        orient=90, align=.5, 'I/F'
polyfill, [ radi, reverse(radi) ], $
          [ val-_errbar, reverse(val+_errbar) ], noclip=0, color=gray()
oplot, radi, val


