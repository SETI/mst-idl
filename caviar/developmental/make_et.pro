restore, 'stretch.sav'
for jjj=0,n_elements(filenames)-1 do begin
  image_name = filenames[jjj]
  print, '------'
  print, strtrim(jjj,2)+' / '+strtrim(n_elements(filenames)-1,2)+'   '+image_name
  print, '------'
  noplot = 1
  @caviar
  if not keyword_set(_et) then begin
    _et = et
  endif else begin
    _et = [ _et, et ]
  endelse
endfor
save, _et, filename='et.sav'

end
