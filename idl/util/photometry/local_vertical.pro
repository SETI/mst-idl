function local_vertical, radii, coords

; The normal to an ellipsoid is its gradient.  Therefore:
;
;            x^2   y^2   z^2        2*x ^   2*y ^   2*z ^
;     grad ( --- + --- + --- )  =  (--- x + --- y + --- z )
;            a^2   b^2   c^2        a^2     b^2     c^2
;
; where (x,y,z) is the position on the ellipsoid and (a,b,c) are the
; triaxial radii.  Since we're only interested in direction, we can
; divide through by 2.  
;
; Input radii is an nx3 vector giving the three axes of n ellipsoids.
; Input coords is in body-frame polar coordinates (lat,lon,rad).
; Output vertical is in body-frame cartesian coordinates.

n = (size(coords))[1]

xyz = polar_to_cart( coords )

vertical = xyz / radii / radii

return, vertical/rebin(v_mag(vertical),n,3)

end