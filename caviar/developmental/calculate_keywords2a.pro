; After running calculate_keywords.pro

; Azimuthal and radial unit vectors
azvec = v_cross(vertical,planet_coord-aimpoint_coord) / rebin(v_mag(planet_coord-aimpoint_coord),nazvec,3)
nazvec = n_elements(azvec[*,0])
radvec = (planet_coord-aimpoint_coord)/rebin(v_mag(planet_coord-aimpoint_coord),nazvec,3)
; Projection of azimuthal and radial vectors onto image plane
azproj = azvec - rebin(v_inner(azvec,camera_coord-aimpoint_coord),nazvec,3)*(camera_coord-aimpoint_coord)/rebin(v_mag(camera_coord-aimpoint_coord),nazvec,3)^2
radproj = radvec - rebin(v_inner(radvec,camera_coord-aimpoint_coord),nazvec,3)*(camera_coord-aimpoint_coord)/rebin(v_mag(camera_coord-aimpoint_coord),nazvec,3)^2
; Restore to unit vectors
azproj = azproj / rebin(v_mag(azproj),nazvec,3)
radproj = radproj / rebin(v_mag(radproj),nazvec,3)
; Vector in the image plane perpendicular to projection of azimuthal vector
azperpproj = v_cross( (camera_coord-aimpoint_coord)/rebin(v_mag(camera_coord-aimpoint_coord),nazvec,3), azproj )
azperpproj = azperpproj / rebin(v_mag(azperpproj),nazvec,3)
; Standard radial scale is measured along radproj, but the really useful 
; radial scale is measured along azperpproj.  Thus, divide by cos of the
; angle between azperpproj and radproj to get the modified radial resolution
radial_scale = v_mag(camera_coord-aimpoint_coord)^2 * cam_params[11]*summ*2 / v_mag(v_cross(camera_coord-aimpoint_coord,planet_coord-aimpoint_coord)) * v_mag(planet_coord-aimpoint_coord)
radial_scale_adj = radial_scale / v_inner(azperpproj,radproj)

;end
