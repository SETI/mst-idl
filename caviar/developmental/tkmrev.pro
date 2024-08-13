function tkmrev, rad, flag, thoukm=thoukm

if not keyword_set(flag) then begin
  savefile = '~/Data/caviar/developmental/tkmdefaultflag.sav'
  if keyword_set(findfile(savefile)) then restore, savefile else flag = 1
endif
if flag eq 1 then begin
  return, rad * 1e3
endif else if flag eq 2 then begin
  if not keyword_set(thoukm) then stop, 'Can''t reverse tkm without thoukm.'
  return, rad + thoukm
endif else begin
  help, flag
  print, 'Unrecognized flag value.'
  return, -1
endelse

end
