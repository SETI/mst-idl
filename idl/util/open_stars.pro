pro open_stars, n, inner_ratio, thick=thick
; Once this routine has been run, setting ps=8 in a plot command will 
; plot the points as open stars (n = # points).

if not keyword_set(n) then n = 5
angle = ( 360. / n * findgen(2*n+1) / 2 + 90 )*!dpi/180
offset = 1 - cos( 360. / n / 2 * !dpi / 180 )
if not keyword_set(inner_ratio) then inner_ratio = sin(angle[2])/sin(angle[1])
radius = fltarr(2*n+1) + 1
radius[2*indgen(n)+1] = inner_ratio
if not keyword_set(thick) then thick = 2
usersym, radius*cos(angle), radius*sin(angle) - offset/2, thick=thick

end
