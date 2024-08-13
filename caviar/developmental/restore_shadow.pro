if keyword_set(noshadow) then shadow = 0 else begin
  period = rstrpos( image_name, '.' )
  suffix = strmid( image_name, period, strlen(image_name) )
  if suffix ne '.IMG' then print, 'Image_name suffix is '+suffix+' not .IMG'
  filestart = rstrpos(image_name,'N',period) > rstrpos(image_name,'W',period)
  if filestart eq -1 then stop, 'Image_Name does not contain N or W.'
  ;filestem = strmid( image_name, 0, period )
  filelen = 13
  while (where(strmid(image_name,filestart+filelen,1) eq ['.','_']))[0] eq -1 do filelen=filelen+1
  filestem = strmid( image_name, 0, filestart+filelen )
  savefile = findfile( filestem+'*.shadow' )
  if n_elements(savefile) gt 1 then stop, 'Multiple files '+filestem+'*.shadow'
  if not keyword_set(savefile) then begin
    print, 'Saved shadow information '+filestem+'*.shadow not found.'
    print, 'Type ".run make_shadow" to define a shadow.'
    shadow = 0
  endif else begin
    print, 'Restoring saved shadow information from '+savefile
    print, 'If you want radscan etc. to avoid the shadow, then type cray=shadow'
    restore, savefile
  endelse
endelse

end
