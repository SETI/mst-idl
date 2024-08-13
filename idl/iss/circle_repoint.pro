e = .0001
;e = .01

; Create a circle of radius 1.
nn = 1000
x = findgen(nn+1)/(nn)*2-1
y = sqrt(1-x^2)
x = [ x, reverse(x) ]
y = [ y, -y ]

; Create a plot window centered on !pi/4 (45 degrees)
xr = sqrt(2)/2 + [ -.005, .005 ]
;xr = sqrt(2)/2 + [ -.05, .05 ]
yr = xr

window, xs=800, ys=800
; Plot the circle offset by distance e along the x-axis
plot, x+e, y, /xs, /ys, xr=xr, yr=yr
; Plot the circle centered on the origin
oplot, x, y

; Draw rays at !pi/4, and at +-.05 radians
dth = .005
;dth = .05
ray1 = transpose([ [0,0], [[cos(!pi/4-dth),sin(!pi/4-dth)]*2] ])
ray2 = transpose([ [0,0], [[cos(!pi/4),sin(!pi/4)]*2] ])
ray3 = transpose([ [0,0], [[cos(!pi/4+dth),sin(!pi/4+dth)]*2] ])
oplot, ray1[*,0], ray1[*,1]
oplot, ray2[*,0], ray2[*,1]
oplot, ray3[*,0], ray3[*,1]

; Find intersection points
pts_i = interpol( x[0:nn], $
          y[0:nn] - poly(x[0:nn],poly_fit( ray1[*,0], ray1[*,1], 1 )), 0 )
pts_i = [ pts_i, interpol( x[0:nn], $
          y[0:nn] - poly(x[0:nn],poly_fit( ray2[*,0], ray2[*,1], 1 )), 0 ) ]
pts_i = [ pts_i, interpol( x[0:nn], $
          y[0:nn] - poly(x[0:nn],poly_fit( ray3[*,0], ray3[*,1], 1 )), 0 ) ]
pts_i = [ [pts_i], [sqrt(1-pts_i^2)] ]
pts_o = interpol( x[0:nn]+e, $
          y[0:nn] - poly(x[0:nn]+e,poly_fit( ray1[*,0], ray1[*,1], 1 )), 0 )
pts_o = [ pts_o, interpol( x[0:nn]+e, $
          y[0:nn] - poly(x[0:nn]+e,poly_fit( ray2[*,0], ray2[*,1], 1 )), 0 ) ]
pts_o = [ pts_o, interpol( x[0:nn]+e, $
          y[0:nn] - poly(x[0:nn]+e,poly_fit( ray3[*,0], ray3[*,1], 1 )), 0 ) ]
pts_o = [ [pts_o], [sqrt(1-(pts_o-e)^2)] ]
oplot, pts_i[*,0], pts_i[*,1], ps=4, co=ctred()
oplot, pts_o[*,0], pts_o[*,1], ps=4, co=ctred()

dr = v_mag(pts_o-pts_i)
ddrdtheta = (dr[2]-dr[0]) / (2*dth)

end
