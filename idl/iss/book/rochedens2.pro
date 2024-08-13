; Calculate the Roche critical density over the extent of the four
; known ring systems. 

mp = [ 1898.6, 568.46, 86.832, 102.43, 0.64185, 0.0127, 23.1e-4 ] * 1e24  ; in kg, from MD99 p.531
rp = [ 71398.0d0, 60330, 26200, 25225, 3394, 1137, 764 ]  ; in km, from MD99 p.531
mp = mp * 1e3  ; Convert from kg to g
rp = rp * 1e5  ; Convert from km to cm
rr = findgen(101)/50 + 1

name = [ 'Jupiter', 'Saturn', 'Uranus', 'Neptune', 'Mars', 'Pluto', 'Rhea' ]
np = n_elements(name)
clr = [ ctred(), ctcyan(), ctgreen(), ctblue(), ctorange(), ctpurple(), ctyellow() ]
xy = [ [2,1.3], [2,0.5], [1.7,1.6], [1.35,5], [2,4.1], [2,2.2], [2.5,0.65] ]
align = [0,1,1,0,0,0,0]
rings = [ [122.5,129], [74.49,136.8], [41.8,51.2], [53.2,63], $
          [0,0], [0,0], [0,0] ] / $
        rebin( rotate(rp/1e8,1), 2, np )

if keyword_set(weiss) then gamma = 1.6 else gamma = 0.848

if keyword_set(dolzr) then begin
  lzr, 'rochedens'
  @plot_prepare
  plot_color
  device, /cmyk
endif

!p.multi = [0,2,2]
notn = replicate(' ',20)
plot, [1,3], [0.1,10]*1.6/gamma, xs=5, /ys, /nodata, $
      ytit='Roche Critical Density, g/cm!U3!N', /ylog, /xlog
axis, xaxis=0, xr=10^!x.crange, /xs, xticks=3, xtickv=[1,2,3], $
      xtickn=['1.0', '2.0', '3.0'], xtit='Planetary Radii'
axis, xaxis=1, xr=10^!x.crange, /xs, xticks=3, xtickv=[1,2,3], xtickn=notn
axis, xaxis=0, xr=10^!x.crange, /xs, xticks=9, xtickv=(findgen(9))*.1+1.1, $
      xtickn=notn, xtickle=!p.ticklen/2
axis, xaxis=1, xr=10^!x.crange, /xs, xticks=9, xtickv=(findgen(9))*.1+1.1, $
      xtickn=notn, xtickle=!p.ticklen/2
axis, xaxis=0, xr=10^!x.crange, /xs, xticks=9, xtickv=(findgen(9))*.1+2.1, $
      xtickn=notn, xtickle=!p.ticklen/2
axis, xaxis=1, xr=10^!x.crange, /xs, xticks=9, xtickv=(findgen(9))*.1+2.1, $
      xtickn=notn, xtickle=!p.ticklen/2
for j=0,np-1 do begin
  oplot, rr, 3*mp[j]/gamma/rr^3/rp[j]^3, co=clr[j]
  xyouts, xy[0,j], xy[1,j]*1.6/gamma, align=align[j], name[j], co=clr[j]
  oplot, rings[*,j], co=clr[j], $
         10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*.02*[[j+1,j+1]] )
  oplot, 10^( !x.crange[0] + (!x.crange[1]-!x.crange[0])*.015*[[j+2,j+2]] ), $
         3*mp[j]/gamma/rings[*,j]^3/rp[j]^3, co=clr[j]
endfor

if keyword_set(dolzr) then clzr

end
