if keyword_set(linsam) then linsam = '_linsam' else linsam = ''
period = rstrpos( image_name, '.' )
filestem = strmid( image_name, 0, period )
f = findfile( filestem+'.edge*'+linsam )
if not keyword_set(f) then savefile = filestem+'.edge1'+linsam else begin
  num = intarr(n_elements(f))
  for j=0,n_elements(f)-1 do num[j] = strmid( f[j], strpos(f[j],'edge')+4, 100 )
  foo = (where( num eq max(num) ))[0]
  savefile = f[foo]
  num = strtrim( fix(num[foo])+1, 2 )
  savefile = strmid( savefile, 0, strpos(savefile,'edge')+4 ) + num + linsam
endelse
print, 'Saving edgefit information to '+savefile
if keyword_set(linsam) then begin
  redge_cmat_linsam = cmat
  save, redge_linsam, redge_sigma_linsam, redge_cmat_linsam, filename=savefile
endif else begin
  redge_cmat = cmat
  save, redge, redge_sigma, redge_cmat, filename=savefile
endelse

end
