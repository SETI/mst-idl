pro solid_circles, radius=radius
; Once this routine has been run, setting ps=8 in a plot command will 
; plot the points as solid circles.

if not keyword_set(radius) then radius = 1
usersym, cos(findgen(49)*7.5*!pi/180)*radius, $
         sin(findgen(49)*7.5*!pi/180)*radius, /fill

end
