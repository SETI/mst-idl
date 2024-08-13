; match = 1
; .run prop_reproj
; matchrad = prop_reproj[foo].radlon[4]
; retall
; match = 0
; nomatch = 1
; .run prop_reproj
; nomatchrad = prop_reproj[foo].radlon[4]
; retall
; nomatch = 0
; cantmatch = 1
; .run prop_reproj
; cantmatchrad = prop_reproj[foo].radlon[4]
; retall
; save, matchrad, nomatchrad, cantmatchrad, filename='matchrad.sav'

binsize = 500
rmin = long( min([ matchrad, nomatchrad, cantmatchrad ]) )
rmax = long( max([ matchrad, nomatchrad, cantmatchrad ]) + 1 )
rmin = rmin - (rmin mod binsize)
rmax = rmax + binsize - (rmax mod binsize)
nrbin = ( rmax - rmin )/binsize + 1
rbin = findgen(nrbin)/(nrbin-1)*(rmax-rmin) + rmin
rx = rebin([ [rbin[0:nrbin-2]], [rbin[1:nrbin-1]] ],nrbin-1,1)

my = lonarr(nrbin-1)
nmy = lonarr(nrbin-1)
cmy = lonarr(nrbin-1)

for j=0,nrbin-2 do begin
  foo = where( matchrad ge rbin[j] and matchrad lt rbin[j+1], count )
  if count gt 0 then my[j] = count
  foo = where( nomatchrad ge rbin[j] and nomatchrad lt rbin[j+1], count )
  if count gt 0 then nmy[j] = count
  foo = where( cantmatchrad ge rbin[j] and cantmatchrad lt rbin[j+1], count )
  if count gt 0 then cmy[j] = count
endfor

if keyword_set(dolzr) then begin
  spawn, 'pwd', pwd
  lslashes = rstrpos( pwd, '/' )
  lslashes = [ lslashes, rstrpos( pwd, '/', lslashes ) ]
  lzr, 'prop_matchrad_'+strmid( pwd, lslashes[1]+1, lslashes[0]-lslashes[1]-1 )+$
       strlowcase(strmid( pwd, lslashes[0]+1, 1000 )), /port
  !p.multi = [0,2,2]
  @plot_prepare
endif

plot, /nodata, tkm( rbin[[0,nrbin-1]] + binsize*[-1,1] ), $
      [0,max(my+nmy+cmy)*1.1], /xs, /ys, $
      xtit='Radius'+tkmtit(), ytit='# of propellers'
for j=0,nrbin-2 do begin
  oplot, tkm(rbin[[j,j+1,j+1,j,j]]), (my[j]+nmy[j]+cmy[j])*[0,0,1,1,0]
  polyfill, tkm(rbin[[j,j+1,j+1,j,j]]), my[j]*[0,0,1,1,0]
endfor

if keyword_set(dolzr) then clzr

end
