pro small_crosses, thick=thick
; Once this routine has been run, setting ps=8 in a plot command will 
; plot the points as half-size crosses.

if not keyword_set(thick) then thick = 2
usersym, [-1,0,0,0,1,0,0,0,-1]/2.0d0, [0,0,1,0,0,0,-1,0,0]/2.0d0, thick=thick

end
