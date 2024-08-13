if not keyword_set(thoukm) then thoukm = min(radi) - (min(radi) mod 1000)
xtit = 'Radius - '+strtrim(long(thoukm),2)+' km'
if not keyword_set(tit) then tit=''
plot, radi-thoukm, val, /xs, /ys, xtit=xtit, ytit='I/F', tit=tit
for j=0,n_elements(rts)-2 do begin
  fsine, radi[rts[j]:rts[j+1]]-thoukm, aa[*,j], f
  oplot, radi[rts[j]:rts[j+1]]-thoukm, f, thick=2, l=2
endfor

end
