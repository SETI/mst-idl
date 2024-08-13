pro solid_stars, n, inner_ratio, thick=thick, scale=scale
; Once this routine has been run, setting ps=8 in a plot command will 
; plot the points as solid stars (n = # points).

if not keyword_set(n) then n = 5
angle = ( 360. / n * findgen(2*n+1) / 2 + 90 )*!dpi/180
offset = 1 - cos( 360. / n / 2 * !dpi / 180 )
if not keyword_set(inner_ratio) then inner_ratio = sin(angle[2])/sin(angle[1])
radius = fltarr(2*n+1) + 1.5  ;Star seems too small when radius=1
radius[2*indgen(n)+1] = inner_ratio
if not keyword_set(scale) then scale = 1
radius = radius*scale
usersym, radius*cos(angle), radius*sin(angle) - offset/2, /fill

end
