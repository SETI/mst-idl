pro fix_badlines, image_name, im, nobadlines=nobadlines

if keyword_set(nobadlines) then return

period = strpos( /reverse_search, image_name, '.' )
;if strmid(image_name,period,1e3) ne '.IMG' then $
;	stop, 'Image_Name extension is not .IMG'
filestart = strpos( /reverse_search, image_name, 'N', period ) > $
            strpos( /reverse_search, image_name, 'W', period )
if filestart eq -1 then stop, 'Image_Name does not contain N or W.'
;filestem = strmid( image_name, 0, period )
filelen = 13
while (where(strmid(image_name,filestart+filelen,1) eq ['.','_']))[0] eq -1 do filelen=filelen+1
filestem = strmid( image_name, 0, filestart+filelen )

if not keyword_set(im) then im = read_vicar( image_name )
nl = (size(im))[1]
savefile = findfile( filestem+'*.bl' )
if n_elements(savefile) gt 1 then stop, 'Multiple files '+filestem+'*.bl'
if keyword_set(savefile) then begin
  print, 'Correcting for badlines as described in '+savefile
  restore, savefile
endif else begin
  print, 'No badlines information restored.'
  return
endelse
; Cannot fix bl at top (j=nl-1) or bottom (j=0).
for j=0,nl-1 do begin
  if bl[j] ge 0 then begin
    if j ne 0 and j ne nl-1 then begin
      if bl[j-1] eq -1 and bl[j+1] eq -1 then begin
        im[bl[j]:nl-1,j] = ( im[bl[j]:nl-1,j-1] + im[bl[j]:nl-1,j+1] )/2
      endif else im[bl[j]:nl-1,j] = 0
    endif else im[bl[j]:nl-1,j] = 0
  endif
endfor

end
