; With an image opened in Caviar, start with a line and sample, translate it to
; RA/Dec, then back to a line and sample. 

if not keyword_set(partial) then partial = 0
common block2,_image_name,swv,planet_id
_image_name = image_name
planet_id = 699L

coords0 = [ [511.5d0,100], [511.5d0,100] ]
nn = 2
coords0 = [ [511.5d0], [511.5d0] ]
nn = 1
if partial eq 1 then begin
  p2radec, cam_params, cmat, nl, coords0[*,0], coords0[*,1], ra0, dec0
  image_coords, long([ [replicate(1,nn)], [ra0], [dec0] ]*3600.0d3), $
                cmat, vobs_planet, cam_params, nl, coords1
  text = 'Input coords:   [ '+string(coords0[0,0],fo='(F10.6)')+', '+$
                              string(coords0[0,1],fo='(F10.6)')+' ]'
  for j=1,nn-1 do text = text + ',   [ ' +$
                              string(coords0[1,0],fo='(F10.6)')+', '+$
                              string(coords0[1,1],fo='(F10.6)')+' ]'
  print, text
  text = 'Output coords:  [ '+string(coords1[0,0],fo='(F10.6)')+', '+$
                              string(coords1[0,1],fo='(F10.6)')+' ]'
  for j=1,nn-1 do text = text + ',   [ ' +$
                              string(coords1[1,0],fo='(F10.6)')+', '+$
                              string(coords1[1,1],fo='(F10.6)')+' ]'
  print, text
endif else if partial eq 2 then begin
  ; This is specific to SOI/ARINGLIT, image 13 (N1467351325)
  rad0 = 133577.4d0
  lon0 = 31.750379d0
  nn = 1
  get_ring, et, rad0, lon0, lon0, polera, poledec, 1, ring, 699L, -82L
  ring = ring[0,*]
  ra = (ring/3600.0d3)[1] + 360
  dec = (ring/3600.0d3)[2]
  p2ralon, cmat, et, polera, poledec, sc, ra, dec, rad1, lon1
  print, 'Input radius & longitude:   [ '+string(rad0,fo='(F10.3)')+', '+$
                                          string(lon0,fo='(F10.6)')+' ]'
  print, 'Output radius & longitude:  [ '+string(rad1,fo='(F10.3)')+', '+$
                                          string(lon1,fo='(F10.6)')+' ]'
endif else if partial eq 3 then begin
  ; This instead uses the RA and Dec that came from the first partial
  p2ralon, cmat, et, polera, poledec, sc, ra0, dec0, rad0, lon0
  get_ring, et, rad0, lon0, lon0, polera, poledec, nn, ring, 699L, -82L;, lons=lon0
  ra1 = (ring/3600.0d3)[*,1]
  dec1 = (ring/3600.0d3)[*,2]
  foo = where( ra1 lt 0, count )
  if count gt 0 then ra1[foo] = ra1[foo] + 360
  foo = where( dec1 lt 0, count )
  if count gt 0 then dec1[foo] = dec1[foo] + 360
  text = 'Input RA & Dec:   [ '+string(ra0[0],fo='(F10.6)')+', '+$
                                string(dec0[0],fo='(F10.6)')+' ]'
  for j=1,nn-1 do text = text + ',   [ ' +$
                                string(ra0[1],fo='(F10.6)')+', '+$
                                string(dec0[1],fo='(F10.6)')+' ]'
  print, text
  text = 'Output RA & Dec:  [ '+string(ra1[0],fo='(F10.6)')+', '+$
                                string(dec1[0],fo='(F10.6)')+' ]'
  for j=1,nn-1 do text = text + ',   [ ' +$
                                string(ra1[1],fo='(F10.6)')+', '+$
                                string(dec1[1],fo='(F10.6)')+' ]'
  print, text
endif else begin
  p2radec, cam_params, cmat, nl, coords0[*,0], coords0[*,1], ra0, dec0
  p2ralon, cmat, et, polera, poledec, sc, ra0, dec0, rad0, lon0
  get_ring, et, rad0, lon0, lon0, polera, poledec, nn, ring, 699L, -82L;, lons=lon0
  ra1 = (ring/3600.0d3)[*,1]
  dec1 = (ring/3600.0d3)[*,2]
  foo = where( ra1 lt 0, count )
  if count gt 0 then ra1[foo] = ra1[foo] + 360
  foo = where( dec1 lt 0, count )
  if count gt 0 then dec1[foo] = dec1[foo] + 360
  image_coords, long([ [replicate(1,nn)], [ra1], [dec1] ]*3600.0d3), $
                cmat, vobs_planet, cam_params, nl, coords1
  text = 'Input coords:   [ '+string(coords0[0,0],fo='(F10.6)')+', '+$
                              string(coords0[0,1],fo='(F10.6)')+' ]'
  for j=1,nn-1 do text = text + ',   [ ' +$
                              string(coords0[j,0],fo='(F10.6)')+', '+$
                              string(coords0[j,1],fo='(F10.6)')+' ]'
  print, text
  text = 'Output coords:  [ '+string(coords1[0,0],fo='(F10.6)')+', '+$
                              string(coords1[0,1],fo='(F10.6)')+' ]'
  for j=1,nn-1 do text = text + ',   [ '+$
                              string(coords1[1,0],fo='(F10.6)')+', '+$
                              string(coords1[1,1],fo='(F10.6)')+' ]'
  print, text
endelse

end
