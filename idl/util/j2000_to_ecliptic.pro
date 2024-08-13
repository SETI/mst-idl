function j2000_to_ecliptic, v, obliquity, complement=complement

; In the J2000 coordinate system, the z-axis is the rotational pole of Earth,
; while the x-axis is the direction of the vernal equinox (the ascending 
; intersection between the ecliptic plane and Earth's equatorial plane).  
; The above is SPICE's J2000 coordinate system.  The coordinate system used
; by JPL Horizons has the z-axis at the orbital pole of Earth instead of the
; rotational pole.  

; Note that the same result can be accomplished using j2000_to_saturn
; with polera = 270 and poledec = 90 - obliquity

if not keyword_set(obliquity) then begin
  obliquity = j2000_earth_obliquity(complement=complement)
endif

xaxis = [ [1], [0], [0] ]
if keyword_set(complement) then q=-1 else q=1
obliquity = q*obliquity * !dpi / 180
szv = size(v)
if szv[0] ne 2 then stop, 'v must have 2 dimensions.'
if szv[2] ne 3 and szv[2] ne 6 then stop, 'v must be an n-by-3 or n-by-6 vector.'
if szv[1] gt 1 then xaxis = rebin( xaxis, szv[1], 3 )

; Alternatively:  cspice_rotvec, reform(v), obliquity, 1, out
if szv[2] eq 3 then begin
  out = v_rotate( v, xaxis, [sin(obliquity)], [cos(obliquity)] )
endif else begin
  out1 = v_rotate( v[*,0:2], xaxis, [sin(obliquity)], [cos(obliquity)] )
  out2 = v_rotate( v[*,3:5], xaxis, [sin(obliquity)], [cos(obliquity)] )
  out = [ [out1], [out2] ]
endelse

return, out

end
