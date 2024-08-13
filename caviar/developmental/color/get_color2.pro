pro get_color2

tvlct,r,g,b,/get
sub = lindgen(249)*(256/249)
; Use v=128 rather than 255 for muted colors.
; Position 255 must remain white for regular plotting.
; In position 250, substitute gray for red. 
; In position 251, substitute ltgray for green.
; In position 252, substitute orange for blue.
; In position 253, substitute ltorange for yellow.
v = 255
u = 160;128  ; Lighter shade (for gray)
t = 210      ; Even lighter shade (for ltgray)
r = [r[sub], 0,160,210,255,255,v,v]
g = [g[sub], v,160,210,128,168,0,v]
b = [b[sub], v,160,210,0,  84, v,v]
tvlct, r, g, b

end
