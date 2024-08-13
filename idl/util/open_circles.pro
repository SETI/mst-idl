pro open_circles, radius=radius, thick=thick
; Once this routine has been run, setting ps=8 in a plot command will 
; plot the points as open circles.

if not keyword_set(radius) then radius = 1
if not keyword_set(thick) then thick = 2
usersym, cos(findgen(49)*7.5*!pi/180)*radius, $
         sin(findgen(49)*7.5*!pi/180)*radius, thick=thick

end
