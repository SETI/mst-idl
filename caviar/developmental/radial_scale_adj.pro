restore, 'et.sav'
restore, 'stretch.sav'
np = n_elements(filenames)
_jjj = lonarr(np)
_radial_scale = dblarr(np)
_radial_scale_adj = dblarr(np)
_radial_scale_cen = dblarr(np)
_lon_scale = dblarr(np)
_lon_scale_cen = dblarr(np)
cspice_furnsh, getenv("CAVIAR_KERNELS")

for j=0l,np-1 do begin

  image_name = filenames[j]
  print, strtrim(j,2)+' / '+strtrim(np,2)+'   '+image_name
  ;rawim = read_vicar( image_name )
  ;fix_badlines, image_name, rawim
  filestem = strmid( image_name, 0, strpos(image_name,'.IMG') )
  jjj = (where( filestem eq $
                strmid( filenames, 0, strlen(filestem) ), count ))[0]
  _jjj[j] = jjj
  if count ne 1 then stop, 'Not a single match to filenames.'
  ;if not keyword_set(propsuff) then propsuff = '.cray'
  ;restore, filestem+propsuff
  offsetfile = filestem + '.offset'
  @get_cam_params
  if keyword_set(findfile(offsetfile)) then restore, offsetfile else begin
    openr, 1, strmid( filestem, 0, strlen(filestem)-4 ) + '.LBL'
    aa = ''
    label = [ [['EXPOSURE_DURATION',''], ['INSTRUMENT_ID',''], $
               ['SPACECRAFT_CLOCK_STOP_COUNT',''], ['NL','']] ]
    readf, 1, aa
    while strmid(aa,0,17) ne label[0,0] do readf, 1, aa
    equal = strpos(aa,'=')
    label[1,0] = strmid(aa,equal+1,1000)
    while strmid(aa,0,13) ne label[0,1] do readf, 1, aa
    equal = strpos(aa,'=')
    label[1,1] = strmid(aa,equal+1,1000)
    while strmid(aa,0,27) ne label[0,2] do readf, 1, aa
    equal = strpos(aa,'=')
    label[1,2] = strmid(aa,equal+1,1000)
    while strmid(aa,0,11) ne '      LINES' do readf, 1, aa
    equal = strpos(aa,'=')
    label[1,3] = strmid(aa,equal+1,1000)
    close, 1
    for k=0,3 do begin
      quotes1 = strpos( label[1,k], '"' )
      if quotes1 ne -1 then begin
        quotes2 = rstrpos( label[1,k], '"' )
        label[1,k] = strmid( label[1,k], quotes1+1, quotes2-quotes1-1 )
      endif 
    endfor 
    image_data, label, _et[jjj], epoch, exposure, cam_name, pmat, nl, found
    cmat = nacmat ## pmat
  endelse 
  et = _et[jjj]
  if nl eq 1024 then summ=1.0d0
  if nl eq 512 then summ=2.0d0
  if nl eq 256 then summ=4.0d0

  ; Define a column vector with the spacecraft's position in J2000 coordinates,
  ; centered on Saturn.
  cspice_spkez,-82L,et,'J2000','NONE',699L,state,ltime
  camera_coord = rotate(state[0:2],1)
  ; Define a column vector with Saturn's position in J2000 coordinates,
  ; centered on Saturn.
  planet_coord = dblarr(1,3)
  ; For the rings, local vertical is the direction of Saturn's pole in J2000
  vertical = polar_to_cart([ [poledec*!dpi/180], [polera*!dpi/180], [1] ])
  ; Define a column vector with the aimpoint's position in J2000 coordinates,
  ; centered on Saturn.
  aimpoint = [511.5,511.5]
  p2radec,cam_params,cmat,nl,aimpoint[1],aimpoint[0],aimp_ra,aimp_dec
  p2ralon,cmat,et,polera,poledec,sc,aimp_ra,aimp_dec,aimp_radius,aimp_lon
  get_ring,et,aimp_radius,0,0,polera,poledec,1,0,699L,lons=aimp_lon,$
           j2000_xyz=aimpoint_coord
  aimpoint_cen = (nl-1)/2. * [1,1]
  p2radec,cam_params,cmat,nl,aimpoint_cen[1],aimpoint_cen[0],$
          aimp_ra_cen,aimp_dec_cen
  p2ralon,cmat,et,polera,poledec,sc,aimp_ra_cen,aimp_dec_cen,$
          aimp_radius_cen,aimp_lon_cen
  get_ring,et,aimp_radius_cen,0,0,polera,poledec,1,0,699L,lons=aimp_lon_cen,$
           j2000_xyz=aimpoint_coord_cen

  ; From calculate_keywords2.pro
  ; Azimuthal and radial unit vectors
  azvec = v_cross(vertical,planet_coord-aimpoint_coord) / (v_mag(planet_coord-aimpoint_coord))[0]
  radvec = (planet_coord-aimpoint_coord)/(v_mag(planet_coord-aimpoint_coord))[0]
  ; Projection of azimuthal and radial vectors onto image plane
  azproj = azvec - (v_inner(azvec,camera_coord-aimpoint_coord))[0]*(camera_coord-aimpoint_coord)/(v_mag(camera_coord-aimpoint_coord))[0]^2
  radproj = radvec - (v_inner(radvec,camera_coord-aimpoint_coord))[0]*(camera_coord-aimpoint_coord)/(v_mag(camera_coord-aimpoint_coord))[0]^2
  ; Restore to unit vectors
  azproj = azproj / (v_mag(azproj))[0]
  radproj = radproj / (v_mag(radproj))[0]
  ; Vector in the image plane perpendicular to projection of azimuthal vector
  azperpproj = v_cross( (camera_coord-aimpoint_coord)/(v_mag(camera_coord-aimpoint_coord))[0], azproj )
  azperpproj = azperpproj / (v_mag(azperpproj))[0]
  ; Standard radial scale is measured along radproj, but the really useful 
  ; radial scale is measured along azperpproj.  Thus, divide by cos of the
  ; angle between azperpproj and radproj to get the modified radial resolution
  radial_scale = v_mag(camera_coord-aimpoint_coord)^2 * cam_params[11]*summ*2 / v_mag(v_cross(camera_coord-aimpoint_coord,planet_coord-aimpoint_coord)) * v_mag(planet_coord-aimpoint_coord)
  radial_scale_adj = radial_scale / v_inner(azperpproj,radproj)
  lon_scale = v_mag(camera_coord-aimpoint_coord)^2 * cam_params[11]*summ*2 / v_mag(v_cross(camera_coord-aimpoint_coord,v_cross(vertical,planet_coord-aimpoint_coord))) * v_mag(planet_coord-aimpoint_coord)
  radial_scale_cen = v_mag(camera_coord-aimpoint_coord_cen)^2 * cam_params[11]*summ*2 / v_mag(v_cross(camera_coord-aimpoint_coord_cen,planet_coord-aimpoint_coord_cen)) * v_mag(planet_coord-aimpoint_coord_cen)
  lon_scale_cen = v_mag(camera_coord-aimpoint_coord_cen)^2 * cam_params[11]*summ*2 / v_mag(v_cross(camera_coord-aimpoint_coord_cen,v_cross(vertical,planet_coord-aimpoint_coord_cen))) * v_mag(planet_coord-aimpoint_coord_cen)
  
  _radial_scale[j] = radial_scale
  _radial_scale_adj[j] = radial_scale_adj
  _radial_scale_cen[j] = radial_scale_cen
  _lon_scale[j] = lon_scale
  _lon_scale_cen[j] = lon_scale_cen

endfor

save, _jjj, _radial_scale, _radial_scale_adj, _radial_scale_cen, $
      _lon_scale, _lon_scale_cen, filename='radial_scale_adj.sav'

end
