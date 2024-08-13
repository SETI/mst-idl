function j2000_to_saturn, v, polera, poledec

; In the J2000 coordinate system, the z-axis is the rotational pole of Earth,
; while the x-axis is the direction of the vernal equinox (the ascending 
; intersection between the ecliptic plane and Earth's equatorial plane).  
; In the preferred Saturn coordinate system, the z-axis is Saturn's rotational 
; pole, and the x-axis is the ascending intersection between Earth's equatorial
; plane (which is also the J2000 xy-plane) and that of Saturn.

xaxis = [ [1], [0], [0] ]
zaxis = [ [0], [0], [1] ]
pra = (90+polera) * !dpi / 180
pdec = (90-poledec) * !dpi / 180
szv = size(v)
if szv[0] ne 2 then stop, 'v must have 2 dimensions.'
if szv[2] ne 3 then stop, 'v must be an n-by-3 vector.'
if szv[1] gt 1 then begin
  xaxis = rebin( xaxis, szv[1], 3 )
  zaxis = rebin( zaxis, szv[1], 3 )
endif

;cspice_rotvec, reform(v), pra, 3, out
;cspice_rotvec, out, pdec, 1, out
;return, out
return, v_rotate( $
		v_rotate( v, zaxis, [sin(pra)], [cos(pra)] ), $
			xaxis, [sin(pdec)], [cos(pdec)] )

end
