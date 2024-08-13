period = rstrpos( image_name, '.' )
filestem = strmid( image_name, 0, period )
f = findfile( filestem+'.scan*' )
if keyword_set(noradscan) then begin
  print, 'Radial scan not restored because noradscan=1.'
endif else if keyword_set(noplot) then begin
  print, 'Radial scan not restored because noplot=1.'
endif else if not keyword_set(f) then begin
  print, 'No saved scans found for filestem '+filestem
endif else begin
  nf = n_elements(f)
  if nf eq 1 then savefile = f[0] else begin
    num = intarr(nf)
    for j=0,nf-1 do num[j] = strmid( f[j], strpos(f[j],'scan')+4, 100 )
    f = f[sort(num)]
    num = num[sort(num)]
    for j=0,nf-1 do print, num[j], '    ', f[j]
    if keyword_set(restore_radscan_num) then begin
      reply = restore_radscan_num
      if reply lt 1 or reply gt num[nf-1] then begin
        print, 'Invalid value for restore_radscan_num.'
        goto, no_restore_radscan_num
      endif
      reply = (where( num eq reply, count ))[0] + 1
      if count eq 0 then begin
        print, 'Invalid value for restore_radscan_num.'
        goto, no_restore_radscan_num
      endif
      print, 'Multiple saved scans detected.  #'+$
             strtrim(restore_radscan_num,2)+' selected automatically.'
    endif else begin
      no_restore_radscan_num:
      reply = ''
      while reply eq '' do begin
        print, 'Multiple saved scans detected.  Please select a number [1,' + $
               strtrim(num[nf-1],2) + '] '
        read, reply
        reply = fix(reply)
        if reply lt 1 or reply gt num[nf-1] then reply=''
        reply = (where( num eq reply, count ))[0] + 1
        if count eq 0 then reply=''
      endwhile
    endelse
    savefile = f[reply-1]
  endelse
  print, 'Restoring radial scan information from '+savefile
  restore, savefile
  print, radscan_descrip
  if keyword_set(radscan_radial_offset) then begin
    if n_elements(radscan_radial_offset) eq n_elements(filenames) then begin
      jjj = (where( image_name eq filenames, count ))[0]
      if count eq 1 then begin
        radi = radi - radscan_radial_offset[jjj]
        print, 'radi adjusted by radscan_radial_offset[jjj] = '+$
               strtrim(-radscan_radial_offset[jjj],2)+' km'
      endif else print, 'image_name does not match a single entry in '+$
                        'filenames, so no action taken.'
    endif else print, 'Number of elements in radscan_radial_offset does not '+$
                      'match that in filenames, so no action taken.'
  endif 
  @reset_radscan
endelse

end
