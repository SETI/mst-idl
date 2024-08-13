pro open_small_circles, thick=thick
; Once this routine has been run, setting ps=8 in a plot command will 
; plot the points as half-size open circles.

if not keyword_set(thick) then thick = 2
usersym, cos(findgen(49)*7.5*!pi/180)/2, sin(findgen(49)*7.5*!pi/180)/2, $
         thick=thick

end
