; Draw arrows at the middle of an image showing directions to Saturn, 
; orbital motion, and the Sun.
; Written by M.S. Tiscareno, July 2006

if keyword_set(arr_hsize) then _arr_hsize=arr_hsize*!d.x_size/128 else _arr_hsize = !d.x_size/128
if keyword_set(arr_thick) then _arr_thick=arr_thick else _arr_thick = 2
if keyword_set(arr_len) then _arr_len=arr_len else _arr_len = 50
if keyword_set(arr_cthick) then _arr_cthick=arr_cthick else _arr_cthick = 0
if keyword_set(arr_xy) then begin &$
  p2radec,cam_params,cmat,nl,arr_xy[1],arr_xy[0],arr_ra,arr_dec &$
  p2ralon,cmat,et,polera,poledec,sc,arr_ra,arr_dec,arr_radius,arr_lon &$
  arr_radius = arr_radius[0] & arr_lon = arr_lon[0] &$
endif else begin &$
  arr_xy = [ (nl-1)*.5, (nl-1)*.5 ] &$
  arr_radius = keywords[0].ringplane_aimpoint_radius[0] &$
  arr_lon = keywords.ringplane_aimpoint_longitude[0] &$
endelse
if (size(arr_xy))[0] eq 1 then arr_xy = rotate(arr_xy,3)
radscale = keywords.ringplane_aimpoint_radial_scale[0]
lonscale = keywords.ringplane_aimpoint_longitudinal_scale_deg[0]
lonscale_km = keywords.ringplane_aimpoint_longitudinal_scale_km[0]

sunxy = [ [cos(keywords.ringplane_subsolar_longitude*!dpi/180)], $
          [sin(keywords.ringplane_subsolar_longitude*!dpi/180)], [0] ] * $
          mean([radscale,lonscale_km])
arr_sun1 = cart_to_polar( sunxy*_arr_len + $
                  polar_to_cart([[0],[arr_lon*!dpi/180],[arr_radius]]) )
get_ring,et,arr_sun1[2],0,0,polera,poledec,1,arr_sun,699L,$
         lons=arr_sun1[1]*180/!dpi
image_coords,arr_sun,cmat,vobs_planet,cam_params,nl,arr_sun1_coords
sun_len = (_arr_len^2 / v_mag( arr_sun1_coords - arr_xy ))[0]
arr_sun2 = cart_to_polar( sunxy*sun_len + $
                  polar_to_cart([[0],[arr_lon*!dpi/180],[arr_radius]]) )
arr_sun3 = cart_to_polar( sunxy*sun_len*1.3 + $
                  polar_to_cart([[0],[arr_lon*!dpi/180],[arr_radius]]) )
get_ring,et,[arr_sun2[2],arr_sun3[2]],0,0,polera,poledec,1,arr_sun,699L,$
         lons=[arr_sun2[1],arr_sun3[1]]*180/!dpi
image_coords,arr_sun,cmat,vobs_planet,cam_params,nl,arr_sun2_coords

arr_sat = [ [arr_radius - radscale*[0,_arr_len,_arr_len*1.3]], $
            [arr_lon + lonscale*[0,0,0]] ]
get_ring,et,arr_sat[*,0],0,0,polera,poledec,1,arr,699L,lons=arr_sat[*,1]
image_coords,arr,cmat,vobs_planet,cam_params,nl,arr_sat_coords
arr_orb = [ [arr_radius - radscale*[0,0,0]], $
            [arr_lon + lonscale*[0,_arr_len,_arr_len*1.3]] ]
get_ring,et,arr_orb[*,0],0,0,polera,poledec,1,arr,699L,lons=arr_orb[*,1]
image_coords,arr,cmat,vobs_planet,cam_params,nl,arr_orb_coords

arr_coords = [ arr_sat_coords, arr_orb_coords, arr_xy, arr_sun2_coords ]
arr_coords[*,0] = (nl-1) - arr_coords[*,0]  ; Caviar plots upside-down

if not keyword_set(arr_data) then arr_data = 0
if not keyword_set(noplot) then for j=0,6,3 do begin &$
  if j eq 0 then begin &$
    clr = green() &$
    name = 'Saturn' &$
  endif else if j eq 3 then begin &$
    clr = blue() &$
    name = 'Orbital!CMotion' &$
  endif else begin &$
    clr = yellow() &$
    name = 'Sun' &$
  endelse &$
  if keyword_set(arr_nocolor) then begin &$
    if keyword_set(arr_dolzr) then clr=0 else clr=ctwhite() &$
  endif &$
  arrow, arr_coords[j,1],arr_coords[j,0],arr_coords[j+1,1],arr_coords[j+1,0], $
         hsize=_arr_hsize, hthick=1, /solid, thick=_arr_thick, color=clr, $
         data=arr_data&$
  xyouts, arr_coords[j+2,1], arr_coords[j+2,0], color=clr, device=1-arr_data, $
          data=arr_data, align=.5, name, charthick=_arr_cthick &$
endfor

;end
