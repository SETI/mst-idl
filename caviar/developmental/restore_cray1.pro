if keyword_set(nocray) then cray = 0 else begin &$
  period = rstrpos( image_name, '.' ) &$
  suffix = strmid( image_name, period, strlen(image_name) ) &$
  if suffix ne '.IMG' then print, 'Image_name suffix is '+suffix+' not .IMG' &$
  filestart = rstrpos(image_name,'N',period) > rstrpos(image_name,'W',period) &$
  if filestart eq -1 then stop, 'Image_Name does not contain N or W.' &$
  ;filestem = strmid( image_name, 0, period ) &$
  filelen = 13 &$
  while (where(strmid(image_name,filestart+filelen,1) eq ['.','_']))[0] eq -1 do filelen=filelen+1 &$
  filestem = strmid( image_name, 0, filestart+filelen ) &$
  savefile = findfile( filestem+'*.cray' ) &$
  if n_elements(savefile) gt 1 then stop, 'Multiple files '+filestem+'*.cray' &$
  if not keyword_set(savefile) then begin &$
    print, 'Saved cosmic ray information '+filestem+'*.cray not found.' &$
    print, 'Type ".run cray_auto" to find cosmic rays automatically, or ".run cray" to do it manually.' &$
    cray = 0 &$
  endif else begin &$
    print, 'Restoring saved cosmic ray information from '+savefile &$
    restore, savefile &$
  endelse &$
endelse

;end
