pro psym_line, angle=angle, radius=radius, thick=thick
; Once this routine has been run, setting ps=8 in a plot command will 
; plot a line of the specified radius and angle. 

if not keyword_set(radius) then radius = 1
if not keyword_set(angle) then angle = 0
usersym, [-1,1]*radius*cos(angle*!pi/180), $
         [-1,1]*radius*sin(angle*!pi/180), thick=thick

end
