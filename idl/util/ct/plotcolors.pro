pro plotcolors

common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

plot,[0,255],[0,300],/xs,/ys,/nodata
oplot,r_orig,ps=4,co=ctred()
oplot,g_orig,ps=4,co=ctgreen()
oplot,b_orig,ps=4,co=ctblue()
oplot,r_curr,co=ctred()
oplot,g_curr,co=ctgreen()
oplot,b_curr,co=ctblue()

clrs = r_curr + 256l*( g_curr + 256l*b_curr ) 
for j=0,255 do polyfill, co=clrs[j], [0,1,1,0,0]+j, [0,0,1,1,0]*40+260

;loadct,0

end