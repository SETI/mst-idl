pro big_circle_cross, thick=thick
; Once this routine has been run, setting ps=8 in a plot command will 
; plot the points as a big circle with a cross in it.

int = 10
ang = findgen(360/int+1)*int * !dpi / 180

if not keyword_set(thick) then thick = 2
usersym, [ cos(ang), -1, 0, 0, 0 ]*2, [ sin(ang), 0, 0, 1, -1 ]*2, thick=thick
;usersym, [ cos(ang) ], [ sin(ang) ]

end
