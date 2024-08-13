pro solid_squares, scale=scale
; Once this routine has been run, setting ps=8 in a plot command will 
; plot the points as solid squares.

if not keyword_set(scale) then scale = 1
usersym, [-1,1,1,-1,-1]*scale, [-1,-1,1,1,-1]*scale, /fill

end
