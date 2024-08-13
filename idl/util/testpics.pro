spawn, 'ls -l', pics
npics = n_elements(pics)
monthmask = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' ]
monthmask1 = [ '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C' ]
pathstem = '~/Personal/home_backup/HomeDirectories/letisca/Pictures/iPhoto\ Library\ test/'
savefile = '~/idl/util/testpics'
if keyword_set(findfile(savefile+'.sav')) then restore, savefile+'.sav' else manual = ''
if keyword_set(findfile(savefile+'_apo.sav')) then restore, savefile+'_apo.sav'
spawn, 'pwd', pwd
for j=1,npics-1 do begin
  spaces = strsplit( pics[j], ' ' )
  nspaces = n_elements(spaces)
  if nspaces lt 9 then stop, spaces
  month = strmid( pics[j], spaces[5], 3 )
  day = strmid( pics[j], spaces[6], 2 )
  yeartime = strmid( pics[j], spaces[7], 4 )
  fname = strmid( pics[j], spaces[8], 1000 )
  if nspaces gt 9 then for k=nspaces-1,9,-1 do begin
    fname = strmid( fname, 0, spaces[k]-spaces[8]-1 ) + '\' + strmid( fname, spaces[k]-spaces[8]-1, 1000 )
  endfor
  if keyword_set(apo) then begin
    apostrophes = strsplit( fname, apo )
    if n_elements(apostrophes) gt 1 then for k=n_elements(apostrophes)-1,1,-1 do begin
      fname = strmid( fname, 0, apostrophes[k]-1 ) + '\' + strmid( fname, apostrophes[k]-1, 1000 )
    endfor
  endif
  paren1 = strsplit( fname, '(' )
  if n_elements(paren1) gt 1 then for k=n_elements(paren1)-1,1,-1 do begin
    fname = strmid( fname, 0, paren1[k]-1 ) + '\' + strmid( fname, paren1[k]-1, 1000 )
  endfor
  paren2 = strsplit( fname, ')' )
  if n_elements(paren2) gt 1 then for k=n_elements(paren2)-1,1,-1 do begin
    fname = strmid( fname, 0, paren2[k]-1 ) + '\' + strmid( fname, paren2[k]-1, 1000 )
  endfor
  amper = strsplit( fname, '&' )
  if n_elements(amper) gt 1 then for k=n_elements(amper)-1,1,-1 do begin
    fname = strmid( fname, 0, amper[k]-1 ) + '\' + strmid( fname, amper[k]-1, 1000 )
  endfor
  day = string( day, fo='(I02)' )
  month = string(where( month eq monthmask )+1,fo='(I02)')
  colon = strpos( yeartime, ':' )
  if colon eq -1 then year = yeartime else year = '2007'
  if not keyword_set(findfile( pathstem+year+'/'+month+'/'+day+'/'+fname )) then begin
    if strmid( fname, 0, 1 ) eq 'P' or strmid( fname, 0, 1 ) eq 'p' then begin
      month = strmid( fname, 1, 1 )
      month = string(where( month eq monthmask1 )+1,fo='(I02)')
      day = strmid( fname, 2, 2 )
      year = strmid( pwd, strpos(pwd,'200'), 4 )
    endif
    if not keyword_set(findfile( pathstem+year+'/'+month+'/'+day+'/'+fname )) then begin
      stop, fname
      if keyword_set(no) then begin
        print, fname+' not added to list of pics that need uploading.'
        no = 0
      endif else manual = [ manual, pwd+'/'+fname ]
    endif
  endif
  next:
endfor
if manual[0] eq '' then manual = clip(manual)
save, manual, filename=savefile+'.sav'

end
