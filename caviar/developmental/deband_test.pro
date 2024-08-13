jn = 50
rawim2 = fltarr(nl,nl,jn)
mndiff = fltarr(jn)
mxdiff = fltarr(jn)
for jj=0,jn-1 do begin
  print,jj
  rawim2[*,*,jj] = deband(rawim,yy=jj)
  mndiff[jj] = mean(abs(clip_edges( rawim - rawim2[*,*,jj], 2 )))
  mxdiff[jj] = max(abs(clip_edges( rawim - rawim2[*,*,jj], 2 )))
endfor

plot, mndiff, /xs, /ylog
oplot, mndiff, co=yellow()
oplot, mxdiff, co=green()

end
