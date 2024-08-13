pro get_color3

tvlct,r,g,b,/get
sub = lindgen(249)*(256/249)
; Add background white to lighten colors
; Position 255 must remain white for regular plotting.
v = 255
w = 168  ; Background white to lighten colors
r = [r[sub], w,v,w,v,v,v,v]
g = [g[sub], v,w,v,v,v,w,v]
b = [b[sub], v,w,w,v,w,v,v]
tvlct, r, g, b

end
