pro get_color

tvlct,r,g,b,/get
sub = lindgen(249)*(256/249)
v = 255
r = [r[sub], 0,v,0,0,v,v,v]
g = [g[sub], v,0,v,0,v,0,v]
b = [b[sub], v,0,0,v,0,v,v]
tvlct, r, g, b

end
