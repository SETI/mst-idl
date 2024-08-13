; For pre-existing prop_reproj.sav, which was done with resfac=4, redo
; the reprojection with resfac=1. 
; Also, since fit_propellers2 has already been run, use fitted centers
; as the centers for the new reprojections. 
restore, 'prop_reproj.sav.4'
restore, 'fit_propellers.sav.4'
npr = n_elements(prop_reproj)
last_image_name = ''
cspice_furnsh, getenv("CAVIAR_KERNELS")

temp = prop_reproj
prop_reproj = { images: temp[0].images, prop: temp[0].prop, $
                primary: temp[0].primary, duplicates: temp[0].duplicates, $
                xy: temp[0].xy, radlon: temp[0].radlon, $
                rrpi: temp[0].rrpi, rrpi4: temp[0].rrpi }
for j=1,npr-1 do begin
  prop_reproj = [ prop_reproj, { images: temp[j].images, prop: temp[j].prop, $
                  primary: temp[j].primary, duplicates: temp[j].duplicates, $
                  xy: temp[j].xy, radlon: temp[j].radlon, $
                  rrpi: temp[j].rrpi, rrpi4: temp[j].rrpi } ]
endfor

for j=0,npr-1 do begin

  image_name = images[ prop_reproj[j].images ]
  print, strtrim(j,2)+' / '+strtrim(npr,2)+'   '+image_name
  if image_name ne last_image_name then begin
    rawim = read_vicar( image_name )
    fix_badlines, image_name, rawim
    filestem = strmid( image_name, 0, strpos(image_name,'.IMG') )
  endif
  resfac = 1

  restore, 'et.sav'
  restore, 'stretch.sav'

  spawn, 'pwd', pwd
  slashes = strsplit( pwd, '/' )
  nslash = n_elements(slashes)
  case strmid( pwd, slashes[nslash-2], 1000 ) of
    'SOI/SOISPTURN': begin
      dr = 1.8d0
      dl = 0.002d0
    end
    '013/ALPSCOOCC': begin
      dr = 10.0d0
      dl = 0.005d0
    end 
    '013/AZSCNHIPH001': begin
      dr = 16.0d0
      dl = 0.015d0
    end 
    '028/RDHRESSCN': begin
      dr = 16.0d0
      dl = 0.015d0
    end 
    '031/RDHRCOMP': begin
      dr = 12.5d0
      dl = 0.009d0
    end 
    '032/RDHRCOMP': begin
      dr = 12.5d0
      dl = 0.009d0
    end 
    '046/RDHRESSCN': begin
      dr = 10.0d0
      dl = 0.005d0
    end 
  endcase 
  ; These files must exist, or prop_anal would not have run
  jjj = (where( filestem eq $
                strmid( filenames, 0, strlen(filestem) ), count ))[0]
  if count ne 1 then stop, 'Not a single match to filenames.'
  restore, filestem+'.offset'
  @get_cam_params
  
  mnrad = prop_reproj[j].radlon[0]
  mxrad = prop_reproj[j].radlon[1]
  mnlon = prop_reproj[j].radlon[2]
  mxlon = prop_reproj[j].radlon[3]
  sz = [ 2.0d0, prop_reproj[j].xy[0:1] ]
  longrid = dindgen(sz[1]+1) / sz[1] * (mxlon-mnlon) + mnlon
  radgrid = dindgen(sz[2]+1) / sz[2] * (mxrad-mnrad) + mnrad
  lonx = rebin( [ [longrid[0:sz[1]-1]], [longrid[1:sz[1]]] ], sz[1], 1 )
  radx = rebin( [ [radgrid[0:sz[2]-1]], [radgrid[1:sz[2]]] ], sz[2], 1 )
  ; Note that interpol( radx, dindgen(sz[2]), (sz[2]-1)/2 ) gives the same 
  ; answer as prop_reproj[j].radlon[4]
  ; Similarly interpol( lonx, dindgen(sz[1]), (sz[1]-1)/2 ) gives the same 
  ; answer as prop_reproj[j].radlon[5]
  xy_rrpi = _pp[4:5,j]
  prop_rad = interpol( radx, dindgen(sz[2]), xy_rrpi[1] )
  prop_lon = interpol( lonx, dindgen(sz[1]), xy_rrpi[0] )

  get_ring, _et[jjj], prop_rad, 0, 0, polera, poledec, n_elements(prop_lon), $
            props, 699l, lons=prop_lon
  image_coords, props, cmat, [[0],[0],[0]], cam_params, nl, coords
  mnrad = prop_rad - dr
  mxrad = prop_rad + dr
  mnlon = prop_lon - dl
  mxlon = prop_lon + dl
  rx = 0 & ry = 0
  caviar_reproject, byte(rawim*0), rawim, mnrad, mxrad, mnlon, mxlon, $
                    _et[jjj], polera, poledec, cam_params, nl, cmat, $
                    [[0],[0],[0]], ry, rx, 0, resfac, /silent, /noplot, $
                    raw_reprojected_image=rrpi
  caviar_reproject, byte(rawim*0), rawim, mnrad, mxrad, mnlon, mxlon, $
                    _et[jjj], polera, poledec, cam_params, nl, cmat, $
                    [[0],[0],[0]], 0, 0, 0, resfac*4, /silent, /noplot, $
                    raw_reprojected_image=rrpi4

  prop_reproj[j].xy = [ rx, ry, round([ coords[0], coords[1] ]) ]
  prop_reproj[j].radlon = [ mnrad, mxrad, mnlon, mxlon, prop_rad, prop_lon ]
  prop_reproj[j].rrpi = ptr_new(rrpi)
  prop_reproj[j].rrpi4 = ptr_new(rrpi4)
  last_image_name = image_name

endfor

save, images, prop_reproj, filename='prop_reproj.sav'

end
