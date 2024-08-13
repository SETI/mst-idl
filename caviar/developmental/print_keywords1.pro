if not keyword_set(silent) then print, '' &$
if not keyword_set(silent) then print, 'IMAGE GEOMETRY' &$
if not keyword_set(silent) then print, '--------------' &$
if keywords.ringplane_minimum_radius eq 0 then fo1 = "(F4.2)" else fo1 = "(F"+strtrim(4+fix(alog10(keywords.ringplane_minimum_radius)),2)+".2)" &$
if keywords.ringplane_maximum_radius eq 0 then fo2 = "(F4.2)" else fo2 = "(F"+strtrim(4+fix(alog10(keywords.ringplane_maximum_radius)),2)+".2)" &$
if not keyword_set(silent) then print, 'Radius range:                    '+string(keywords.ringplane_minimum_radius,fo="(F"+strtrim(4+fix(alog10(keywords.ringplane_minimum_radius)),2)+".2)")+' to '+string(keywords.ringplane_maximum_radius,fo="(F"+strtrim(4+fix(alog10(keywords.ringplane_maximum_radius)),2)+".2)")+' km' &$
if not keyword_set(silent) then print, 'Longitude range:                 '+strtrim(keywords.ringplane_least_orbital_longitude,2)+' to '+strtrim(keywords.ringplane_greatest_orbital_longitude,2)+' degrees' &$
if keywords.ringplane_aimpoint_radius eq 0 then fo3 = "(F4.2)" else fo3 = "(F"+strtrim(4+fix(alog10(keywords.ringplane_aimpoint_radius)),2)+".2)" &$
if not keyword_set(silent) then print, 'Aimpoint radius and longitude:   '+string(keywords.ringplane_aimpoint_radius,fo="(F"+strtrim(4+fix(alog10(keywords.ringplane_aimpoint_radius)),2)+".2)")+' km, '+strtrim(keywords.ringplane_aimpoint_longitude,2)+' degrees' &$
if not keyword_set(silent) then print, 'Aimpoint incidence angle:        '+strtrim(keywords.ringplane_aimpoint_incidence_angle,2)+' degrees' &$
if not keyword_set(silent) then print, 'Aimpoint emission angle:         '+strtrim(keywords.ringplane_aimpoint_emission_angle,2)+' degrees' &$
if not keyword_set(silent) then print, 'Aimpoint phase angle:            '+strtrim(keywords.ringplane_aimpoint_phase_angle,2)+' degrees' &$
if not keyword_set(silent) then print, 'Radial scale at aimpoint:        '+strtrim(keywords.ringplane_aimpoint_radial_scale,2)+' km/pixel' &$
if not keyword_set(silent) then print, 'Longitudinal scale at aimpoint:  '+strtrim(keywords.ringplane_aimpoint_longitudinal_scale_km,2)+' km/pixel = '+strtrim(keywords.ringplane_aimpoint_longitudinal_scale_deg,2)+' degrees/pixel' &$

if not keyword_set(silent) then print, 'This image looks at the '+ldside+' side of the ring plane.' &$
if not keyword_set(silent) then print, 'The current aimpoint is [ '+strtrim(aimpoint[0],2)+', '+strtrim(aimpoint[1],2)+' ]. Type @aimpoint to change.' &$
if not keyword_set(silent) then print, '' &$


