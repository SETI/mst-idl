function tkmtit, rad, flag, thoukm=thoukm, runits=runits

if not keyword_set(flag) then begin
  savefile = '~/Data/caviar/developmental/tkmdefaultflag.sav'
  if keyword_set(findfile(savefile)) then restore, savefile else flag = 1
  if keyword_set(thoukm) then tkmflag = 2
endif
;@tkm_check_for_small_ints
if flag eq 1 or flag eq 3 then begin
  ;if not keyword_set(runits) then runits = 'kkm'
  if not keyword_set(runits) then runits = '1000 km'
  return, ' ('+runits+')'
endif else if flag eq 2 then begin
  if not keyword_set(thoukm) then thoukm = get_thoukm( rad, tkmbase )
  if not keyword_set(runits) then runits = 'km'
  if thoukm eq 0 then begin
    return, ' ('+runits+')'
  endif else begin
    return, ' - '+strtrim(long(thoukm),2)+' '+runits
  endelse
endif else begin
  help, flag
  print, 'Unrecognized flag value.'
  return, -1
endelse

end
