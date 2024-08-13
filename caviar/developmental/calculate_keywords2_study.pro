; After running calculate_keywords.pro

theta = findgen(3601)/10 * !pi/180
nth = n_elements(theta)

camaim = camera_coord - aimpoint_coord
planaim = planet_coord - aimpoint_coord

a=v_mag(camaim)
b=cam_params[11]*summ*2
c=v_mag(v_cross(camaim,planaim))
d=v_mag(planaim)
e=v_mag(v_cross(camaim,v_cross(vertical,planaim)))

; Radial and longitudinal scales
print, a^2*b*d/c
print, a^2*b*d/e

azvec = v_cross(vertical,planaim) / d[0]
radvec = planaim/d[0]
; Verify dot product is zero, cross product has magnitude one
print,v_inner(azvec,radvec)
print,v_mag(v_cross(azvec,radvec))
; Again radial and longitudinal scales
print, a^2*b/v_mag(v_cross(camaim,radvec))
print, a^2*b/v_mag(v_cross(camaim,azvec))

res=a[0]^2*b/v_mag(v_cross(rebin(camaim,nth,3),v_rotate(rebin(azvec,nth,3),rebin(vertical,nth,3),sin(theta),cos(theta))))
plot, theta*180/!pi, res, /ys
print, !y.crange

; Projection of azimuthal direction onto image plane
azproj = azvec - (v_inner(azvec,camaim))[0]*camaim/a[0]^2
radproj = radvec - (v_inner(radvec,camaim))[0]*camaim/a[0]^2
;; Restore to unit vectors
azproj = azproj / (v_mag(azproj))[0]
radproj = radproj / (v_mag(radproj))[0]
; Perpendicular to projection of azimuthal vector
azperpproj = v_cross( camaim/a[0], azproj )
azperpproj = azperpproj / (v_mag(azperpproj))[0]
; Interstingly, projecting this onto the ring plane gives you radvec.
radvec2 = azperpproj - (v_inner(azperpproj,vertical))[0]*vertical
radvec2 = radvec2 / (v_mag(radvec2))[0]
; To re-obtain azvec, find the intersection of the plane containing
; both azproj and camaim with the ring plane, which is the line
; perpendicular both to azperpproj and to vertical. 
azvec2 = v_cross( planaim/d[0], azperpproj )
azvec2 = azvec2 / (v_mag(azvec2))[0]
; By analogy, the direction in the ring plane that projects onto the
; image plane to give the direction perpendicular to azproj is given by
azperp = v_cross( planaim/d[0], azproj )
azperp = azperp / (v_mag(azperp))[0]
; Finally, resolution along this direction
print, a^2*b/v_mag(v_cross(camaim,azperp))

; Not sure why that didn't work, but I think this does:
; Angle between azproj and radproj, then between azperpproj and radproj
print,acos(v_inner(azproj,radproj))*180/!pi
print,acos(v_inner(azperpproj,radproj))*180/!pi
; Since radial scale is measured along radproj, divide by cos of the
; latter to get the modified radial resolution
print,a^2*b*d/c/v_inner(azperpproj,radproj)

end
