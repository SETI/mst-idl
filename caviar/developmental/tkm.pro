function tkm, rad, flag, thoukm=thoukm, phi=phi

if keyword_set(phi) then return, rad
if not keyword_set(flag) then begin
  savefile = '~/Data/caviar/developmental/tkmdefaultflag.sav'
  if keyword_set(findfile(savefile)) then restore, savefile else flag = 1
  if keyword_set(thoukm) then flag = 2
endif
if flag eq -1 then return, rad
;@tkm_check_for_small_ints
if flag eq 1 or flag eq 3 then begin
  return, rad / 1e3
endif else if flag eq 2 then begin
  if not keyword_set(thoukm) then thoukm = get_thoukm( rad, tkmbase )
  return, rad - thoukm
endif else begin
  help, flag
  print, 'Unrecognized flag value.'
  return, -1
endelse

end
