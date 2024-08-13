function fit_propellers4_subtractavg, rrpi, rrpi_orig, mask, sz, $
                   edgebnd=edgebnd, rrpi_prof=rrpi_prof, profhole=profhole

if not keyword_set(sz) then sz = size(rrpi_orig)
if not keyword_set(mask) then mask = bytarr( sz[1], sz[2] ) + 1
rrpi_prof = rrpi[0,*]*0
for k=0l,sz[2]-1 do begin
  foogood = where( mask[*,k] eq 1, countgood )
  if keyword_set(edgebnd) then begin
    foo = where( foogood le edgebnd[0] or foogood ge edgebnd[1], countgood )
    if countgood gt 0 then foogood = foogood[foo]
  endif 
  if countgood gt 0 then rrpi_prof[k] = median(rrpi_orig[foogood,k])
endfor
if keyword_set(profhole) then begin
  if n_elements(profhole) ne n_elements(rrpi_prof) then stop
  foo = where( profhole, count )
  if count ne 0 then begin
    notfoo = vec_remove( indgen(sz[2]), foo )
    rrpi_prof[foo] = interpol( rrpi_prof[notfoo], notfoo, foo )
  endif 
endif
rrpi = rrpi_orig - rebin( rrpi_prof, sz[1], sz[2] )
rrpi = rrpi*mask

return, rrpi

end
