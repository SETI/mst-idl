period = rstrpos( image_name, '.' )
filestem = strmid( image_name, 0, period )
f = findfile( filestem+'.scan*' )
if not keyword_set(f) then savefile = filestem+'.scan1' else begin
  num = intarr(n_elements(f))
  for j=0,n_elements(f)-1 do num[j] = strmid( f[j], strpos(f[j],'scan')+4, 100 )
  foo = where( num eq max(num) )
  savefile = f[foo]
  num = strtrim( fix(num[foo])+1, 2 )
  savefile = strmid( savefile, 0, strpos(savefile,'scan')+4 ) + num
endelse
print, 'Saving radial scan information to '+savefile
radscan_cmat = cmat
save, radi, val, errbar, radscan_cmat, radscan_np, phiout, radscan_descrip, $
           filename=savefile

end
