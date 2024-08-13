; Find the radius and longitude extrema in the image
if keyword_set(rlminmax_silent) then rlmmsold=rlminmax_silent else rlmmsold=0
rlminmax_silent = 1
@rlminmax_j
rlminmax_silent = rlmmsold

; Define a column vector with the spacecraft's position in J2000 coordinates,
; centered on Saturn.
cspice_spkez,-98L,et,'J2000','NONE',599L,state,ltime
camera_coord = rotate(state[0:2],1)

; Define a column vector with the Sun's position in J2000 coordinates,
; centered on Saturn.
cspice_spkez,10L,et,'J2000','NONE',599L,state,ltime
sun_coord = rotate(state[0:2],1)

; Define a column vector with the aimpoint's position in J2000 coordinates,
; centered on Saturn.
if keyword_set(specify_aimpoint) and keyword_exists(aimp_radius) and $
  keyword_exists(aimp_lon) then print, 'Using input aimpoint radius = '+$
    strtrim(aimp_radius,2)+' and longitude = '+strtrim(aimp_lon,2) else begin &$
if not keyword_set(aimpoint) then aimpoint = (nl-1)/2. * [1,1] &$
p2radec_quicker_j,pixscale,cmat,nl,aimpoint[1],aimpoint[0],aimp_ra,aimp_dec &$
p2ralon_j,cmat,et,polera,poledec,sc,aimp_ra,aimp_dec,aimp_radius,aimp_lon &$
endelse
get_ring,et,aimp_radius,0,0,polera,poledec,1,0,599L,-98L,lons=aimp_lon,j2000_xyz=aimpoint_coord

; Define a column vector with Saturn's position in J2000 coordinates,
; centered on Saturn.
planet_coord = dblarr(1,3)

; For the rings, local vertical is the direction of Saturn's pole in J2000
vertical = polar_to_cart([ [poledec*!dpi/180], [polera*!dpi/180], [1] ])

;if nl eq 1024 then summ=1.0d0
;if nl eq 512 then summ=2.0d0
;if nl eq 256 then summ=4.0d0

keywords = { $
            ringplane_minimum_radius: _rmin, $
            ringplane_maximum_radius: _rmax, $
            ringplane_least_orbital_longitude: _lmin, $
            ringplane_greatest_orbital_longitude: _lmax, $
            ringplane_aimpoint_radius: aimp_radius, $
            ringplane_aimpoint_longitude: aimp_lon, $
            ringplane_aimpoint_incidence_angle: (incidence_angle( aimpoint_coord, sun_coord, vertical ))[0]*180/!dpi, $
            ringplane_aimpoint_emission_angle: (emission_angle( aimpoint_coord, camera_coord, vertical ))[0]*180/!dpi, $
            ringplane_aimpoint_phase_angle: (phase_angle( aimpoint_coord, camera_coord, sun_coord ))[0]*180/!dpi, $
            ringplane_aimpoint_distance: v_mag(camera_coord-aimpoint_coord), $
            ringplane_aimpoint_radial_scale: v_mag(camera_coord-aimpoint_coord)^2 *pixscale*2 / v_mag(v_cross(camera_coord-aimpoint_coord,planet_coord-aimpoint_coord)) * v_mag(planet_coord-aimpoint_coord), $
            ringplane_aimpoint_longitudinal_scale_km: v_mag(camera_coord-aimpoint_coord)^2 * pixscale*2 / v_mag(v_cross(camera_coord-aimpoint_coord,v_cross(vertical,planet_coord-aimpoint_coord))) * v_mag(planet_coord-aimpoint_coord), $
            ringplane_aimpoint_longitudinal_scale_deg: v_mag(camera_coord-aimpoint_coord)^2 * pixscale*2 / v_mag(v_cross(camera_coord-aimpoint_coord,v_cross(vertical,planet_coord-aimpoint_coord))) * v_mag(planet_coord-aimpoint_coord) / aimp_radius * 180 / !dpi, $
            ringplane_subspacecraft_latitude: (cart_to_polar(j2000_to_saturn(camera_coord,polera,poledec)))[0]*180/!dpi, $
            ringplane_subspacecraft_longitude: (cart_to_polar(j2000_to_saturn(camera_coord,polera,poledec)))[1]*180/!dpi, $
            ringplane_subspacecraft_incidence_angle: (incidence_angle( planet_coord, sun_coord, vertical ))[0]*180/!dpi, $
            ringplane_subspacecraft_emission_angle: (emission_angle( planet_coord, camera_coord, vertical ))[0]*180/!dpi, $
            ringplane_subspacecraft_phase_angle: (phase_angle( planet_coord, camera_coord, sun_coord ))[0]*180/!dpi, $
            ringplane_subsolar_latitude: (cart_to_polar(j2000_to_saturn(sun_coord,polera,poledec)))[0]*180/!dpi, $
            ringplane_subsolar_longitude: (cart_to_polar(j2000_to_saturn(sun_coord,polera,poledec)))[1]*180/!dpi, $
            primary_distance: v_mag(camera_coord) $
            }

if sign(keywords.ringplane_subspacecraft_latitude) eq sign(keywords.ringplane_subsolar_latitude) then ldside='LIT' else ldside='DARK'
;@print_keywords

