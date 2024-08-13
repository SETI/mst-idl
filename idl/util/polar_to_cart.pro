function polar_to_cart, v, float=float

; Input v is an nv-by-3 array of 3-element column vectors in polar
; coordinates (lat,lon,rad).  Result is an nv-by-3 array of 3-element 
; column vectors in Cartesian coordinates (x,y,z).

 sv = size(v)
 nv = sv[1]

 lat=v[*,0,*]
 lon=v[*,1,*]
 rad=v[*,2,*]

 if keyword_set(float) then result = fltarr(nv,3) else result = dblarr(nv,3)
 result[*,0,*] = rad*cos(lat)*cos(lon)
 result[*,1,*] = rad*cos(lat)*sin(lon)
 result[*,2,*] = rad*sin(lat)

 return, result
end
;===========================================================================
