pro open_tiny_circles
; Once this routine has been run, setting ps=8 in a plot command will 
; plot the points as one-tenth-size open circles.

usersym, cos(findgen(49)*7.5*!pi/180)/10, sin(findgen(49)*7.5*!pi/180)/10

end
