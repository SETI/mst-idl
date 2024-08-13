pro open_polygons, n, rotate=rotate, down=down, thick=thick
; Once this routine has been run, setting ps=8 in a plot command will 
; plot the points as open polygons (n = # sides).

if not keyword_set(n) then n = 5
offset = 1 - cos( 360. / n / 2 * !dpi / 180 )
if not keyword_set(rotate) then begin
  if keyword_set(down) then begin
    rotate = -90
    offset = -offset
  endif else rotate = 90
endif
angle = ( 360. / n * findgen(n+1) + rotate )*!dpi/180
if not keyword_set(thick) then thick = 2
usersym, cos(angle), sin(angle) - offset/2, thick=thick

end
