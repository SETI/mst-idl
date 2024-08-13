pro mark_badlines, filename, bl, nocheck=nocheck

if n_params() eq 0 then begin
  print, 'Syntax:  Mark_Badlines, filename, bl'
  print, 'Automatically saves bl, replacing .IMG with .bl in filename.'
  retall
endif

if strpos(filename,'cal') ne -1 then begin
  print, 'Never use a calibrated image to mark badlines!'
  return
endif

period = rstrpos( filename, '.' )
if strmid(filename,period,1e3) ne '.IMG' then $
	stop, 'Filename extension is not .IMG'
filestem = strmid( filename, 0, period )

im = read_vicar( filename )
foo = where( im eq 0, nz )
nl = (size(im))[1]
bl = intarr( nl )
if nz eq 0 then bl[*] = -1 else begin
  for j=0,nl-1 do bl[j] = min(where( im[*,j] eq 0 ))
  if total( nl - bl[where(bl ne -1)] ) ne nz and not keyword_set(nocheck) then begin
    print, 'Lines with zeroes appear to not extend to right-hand edge.'
    print, 'This is not a fatal error.  You might want to look at the image, or just ".c" to continue.'
    print, 'To prevent this situation from being flagged, set nocheck=1.'
    stop, where(bl ne -1)
  endif
endelse

save, bl, filename=filestem+'.bl'

end
