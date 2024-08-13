; restore, 'stretch.sav'
; jjj = -1
if not keyword_set(j1) then j1=0
if not keyword_set(j2) then j2=n_elements(filenames)-1
for jjj=j1,j2 do begin
  image_name=filenames[jjj]
  @caviar
  print, image_name+'  ('+strtrim(jjj,2)+'/'+$
         strtrim(n_elements(filenames),2)+')'
  @ns_radscan
  @save_bksub
endfor

end

