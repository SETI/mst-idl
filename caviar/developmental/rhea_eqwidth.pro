; First .run rhea_coadd
zra = zrarray( image_name, /rhea, ll=ll )
zra[0,*,*] = zra[0,*,*] / 764  ; Convert from km to R_Rhea
;savefile = 'add'+strtrim(minim,2)+'thru'+strtrim(maxim,2)+'_lims.sav'
;if keyword_set(findfile(savefile)) then restore, savefile
if not keyword_set(mnrad) or not keyword_set(mxz) then begin
  print, 'Click on minimum radius and maximum abs(z)'
  cursor, /device, x1, y1, 3
  mnrad = zra[ 0, x1, nl-1-y1 ]
  mxz = zra[ 1, x1, nl-1-y1 ]
endif
dz = _keywords[meanim].ringplane_aimpoint_radial_scale / 3
if n_elements(dz) gt 1 then stop
dz = dz[0]
goodplus = wher( zra[1,*,*] gt 3*mxz-dz and zra[1,*,*] lt 3*mxz+dz )
goodminus = wher( -zra[1,*,*] gt 3*mxz-dz and -zra[1,*,*] lt 3*mxz+dz )
mxrad = max(zra[ 0, goodplus[1,*], goodplus[2,*] ])
mxrad = mxrad < max(zra[ 0, goodminus[1,*], goodminus[2,*] ])

box1 = lonarr(2,4)
box2 = lonarr(2,4)
box3 = lonarr(2,4)
tmp = reform( (abs(zra[0,*,*]-mnrad)+1) * (abs(zra[1,*,*]-mxz)+1) )
box1[*,0] = wher( tmp eq min(tmp) )
box2[*,3] = wher( tmp eq min(tmp) )
tmp = reform( (abs(zra[0,*,*]-mxrad)+1) * (abs(zra[1,*,*]-mxz)+1) )
box1[*,1] = wher( tmp eq min(tmp) )
box2[*,2] = wher( tmp eq min(tmp) )
tmp = reform( (abs(zra[0,*,*]-mnrad)+1) * (abs(zra[1,*,*]+mxz)+1) )
box1[*,3] = wher( tmp eq min(tmp) )
box3[*,0] = wher( tmp eq min(tmp) )
tmp = reform( (abs(zra[0,*,*]-mxrad)+1) * (abs(zra[1,*,*]+mxz)+1) )
box1[*,2] = wher( tmp eq min(tmp) )
box3[*,1] = wher( tmp eq min(tmp) )
tmp = reform( (abs(zra[0,*,*]-mnrad)+1) * (abs(zra[1,*,*]-3*mxz)+1) )
box2[*,0] = wher( tmp eq min(tmp) )
tmp = reform( (abs(zra[0,*,*]-mxrad)+1) * (abs(zra[1,*,*]-3*mxz)+1) )
box2[*,1] = wher( tmp eq min(tmp) )
tmp = reform( (abs(zra[0,*,*]-mnrad)+1) * (abs(zra[1,*,*]+3*mxz)+1) )
box3[*,3] = wher( tmp eq min(tmp) )
tmp = reform( (abs(zra[0,*,*]-mxrad)+1) * (abs(zra[1,*,*]+3*mxz)+1) )
box3[*,2] = wher( tmp eq min(tmp) )
box1 = [ [box1], [box1[*,0]] ]
box2 = [ [box2], [box2[*,0]] ]
box3 = [ [box3], [box3[*,0]] ]
plots, /device, color=cyan(), box1[0,*], nl-1-box1[1,*]
plots, /device, color=yellow(), box2[0,*], nl-1-box2[1,*]
plots, /device, color=yellow(), box3[0,*], nl-1-box3[1,*]

pts1 = polyfillv( box1[0,*], box1[1,*], nl, nl )
pts2 = polyfillv( box2[0,*], box2[1,*], nl, nl )
pts3 = polyfillv( box3[0,*], box3[1,*], nl, nl )
print, 'Ansa longitude ranges:  ' + $
       strtrim( max(ll[pts1])-min(ll[pts1]), 2) + '  ' + $
       strtrim( max(ll[pts2])-min(ll[pts2]), 2) + '  ' + $
       strtrim( max(ll[pts3])-min(ll[pts3]), 2)
ansalon1 = mean(ll[pts1])
ansalon2 = mean(ll[pts2])
ansalon3 = mean(ll[pts3])

radial_scan, rawim, radi, val2, et, polera, poledec, cam_params, nl, cmat, vobs_planet, mnrad=mnrad*764, mxrad=mxrad*764, mnlon=mxz, mxlon=3*mxz, radarray=reform(zra[0,*,*])*764, lonarray=reform(zra[1,*,*]), errbar=errbar, radscan_np=radscan_np, color=yellow(), /vertical, ansalon=ansalon2, primary=605L
radial_scan, rawim, radi, val3, et, polera, poledec, cam_params, nl, cmat, vobs_planet, mnrad=mnrad*764, mxrad=mxrad*764, mnlon=-3*mxz, mxlon=-mxz, radarray=reform(zra[0,*,*])*764, lonarray=reform(zra[1,*,*]), errbar=errbar, radscan_np=radscan_np, color=yellow(), /vertical, ansalon=ansalon3, primary=605L
radial_scan, rawim, radi, val1, et, polera, poledec, cam_params, nl, cmat, vobs_planet, mnrad=mnrad*764, mxrad=mxrad*764, mnlon=-mxz, mxlon=mxz, radarray=reform(zra[0,*,*])*764, lonarray=reform(zra[1,*,*]), errbar=errbar, radscan_np=radscan_np, color=cyan(), /vertical, ansalon=ansalon1, primary=605L

nradi = n_elements(radi)
bg_old = rebin( [[val2],[val3]], nradi, 1 )
q = 2.
bg = interpolate( [[val2],[replicate(0,nradi,q)],[val3]], $
                  findgen(nradi), replicate((q+1)/2,nradi), cubic=-.5 )

if keyword_set(dolzr) then begin
  tiffcolor=1
  tifffile='add'+strtrim(minim,2)+'thru'+strtrim(maxim,2)+'_scan'
  if keyword_set(hipass) then tifffile=tifffile+'_hipass'
  psname=tifffile
  tifffile=tifffile+'.tiff'
  @caviar_tiff1
  lzr, /half, psname
  @plot_prepare
  plot_color
endif else if !d.name eq 'X' then window
!p.multi = [0,1,2]

plot, [min(radi),max(radi)]/764, [min([val1,val2,val3]),max([val1,val2,val3])], $
      /xs, /ys, /nodata, xtit='Distance from Rhea center (R!DRhea!N)', ytit='I/F'
oplot, radi/764, val2, co=yellow()
oplot, radi/764, val3, co=yellow()
oplot, radi/764, bg, co=yellow()
oplot, radi/764, val1, co=cyan()
plot, radi/764, val1 - bg, /xs, /ys, $
      xtit='Distance from Rhea center (R!DRhea!N)', ytit='I/F'
if keyword_set(dolzr) then clzr

end
