pro open_triangles, thick=thick
; Once this routine has been run, setting ps=8 in a plot command will 
; plot the points as open triangles.

if not keyword_set(thick) then thick = 2
usersym, [0,-1,1,0], [1,-1,-1,1], thick=thick

end
