pro solid_small_circles
; Once this routine has been run, setting ps=8 in a plot command will 
; plot the points as half-size solid circles.

usersym, cos(findgen(49)*7.5*!pi/180)/2, sin(findgen(49)*7.5*!pi/180)/2, /fill

end
