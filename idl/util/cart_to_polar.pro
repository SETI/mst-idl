function cart_to_polar, v, float=float

; Input v is an nv-by-3 array of 3-element column vectors in cartesian 
; coordinates (x,y,z).  Result is an nv-by-3 array of 3-element column 
; vectors in polar coordinates (lat,lon,rad).

 sv = size(v)
 nv = sv[1]

 rad=sqrt(total(v*v, 2))

 lat=asin(v[*,2,*]/rad)
 
 lon=atan(v[*,1,*],v[*,0,*])
 w=where(finite(lon) NE 1)
 if(w[0] NE -1) then lon[w]=0.0

 if keyword_set(float) then result = fltarr(nv,3) else result = dblarr(nv,3)
 result[*,0,*] = lat
 result[*,1,*] = lon
 result[*,2,*] = rad

 return, result
end
;===========================================================================



