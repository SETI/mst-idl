@get_cam_params1
vobs_planet = [0.0d0,0.0d0,0.0d0]
cspice_furnsh, getenv("CAVIAR_KERNELS")

if keyword_set(image_mid_time) then use_prop_imtable = 0
if not keyword_exists(use_prop_imtable) then use_prop_imtable = 1
if keyword_set(use_prop_imtable) then begin
  openr, 1, 'prop_imtable_110106.txt'
  aa = ''
  images = ''
  image_mid_time = ''
  nn = 0
  while not eof(1) do begin
    readf, 1, aa
    if strmid(aa,1,1) eq 'N' then begin
      images = [ images, strmid(aa,1,11) ]
      image_mid_time = [ image_mid_time, strmid(aa,21,8)+'T'+strmid(aa,30,8) ]
      nn = nn + 1
    endif
  endwhile 
  close, 1
  images = images[1:nn]
  image_mid_time = image_mid_time[1:nn]
endif

; Test, image N1654284680 containing Bleriot at [line,sample] = [433,598]
if not keyword_set(image_mid_time) then image_mid_time = '2010-154T18:46:09.459'
nn = n_elements(image_mid_time)

lon = dblarr(nn)
planet_coords = dblarr(2,nn)
for j=0,nn-1 do begin
  ; Bleriot orbit
  ; Successful values for lonfit and epoch come from propeller_orbit.pro,
  ; not from propeller_orbit3.pro or Tiscareno et al (2010, ApJL). 
  rad = 134912.24521d0
  ;lonfit = [ 193.65d0, 616.7819329d0 ]
  ;epoch = 2454102.0d0  ; 2007-001T12:00:00
  lonfit = [ 125.42389d0, 616.7819329d0 ]
  epoch = 2454084.6d0  ; 2006-349T02:24:57.183
  ; Should come to longitude = 192.2983 for N1654284680

  cspice_str2et, image_mid_time[j], et
  cspice_sce2c, -82L, et, sclkdp
  cspice_ckgp, -82000L, sclkdp, 1000.0d0, 'J2000', pmat, clkout, found
  cspice_timout, et, 'YYYY-DOYTHR:MN:SC.###::UTC', 21, ctime
  cmat = nacmat ## pmat

  imjd = caldate( image_mid_time[j], /reverse, /doy )
  deltat = imjd - epoch
  lon[j] = fix_angles( lonfit[0] + lonfit[1]*deltat, /deg, /to360 )

  @get_sat_prepare
  planet_sat_sat = [ [rad*cos(lon[j]*!dpi/180)], [rad*sin(lon[j]*!dpi/180)], [0] ]
  planet_sat_j2000 = saturn_to_j2000( planet_sat_sat, polera, poledec )
  cspice_spkez, -82L, et, 'J2000', 'NONE', 699L, state, ltime
  camera_sat_j2000 = rotate(state[0:2],1)
  planet_cam_j2000 = planet_sat_j2000 - camera_sat_j2000
  cspice_recrad, reform(planet_cam_j2000), range, ra, dec
  planet = lonarr(1,3)
  RA = 3600.0d3*RA*360.0d0/(2.0d0*!dpi)
  dec = 3600.0d3*dec*360.0d0/(2.0d0*!dpi)
  planet[0,1] = ra
  planet[0,2] = dec
  image_coords, planet, cmat, vobs_planet, cam_params, nl, _planet_coords
  planet_coords[*,j] = _planet_coords

endfor

if keyword_set(use_prop_imtable) then begin
  savefile = 'imsearch_prop_imtable_110106.sav'
  if keyword_set(findfile(savefile)) then stop, savefile+' exists'
  save, images, image_mid_time, lon, planet_coords, rad, filename=savefile
endif

end
