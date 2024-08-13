rpj_rads = 0
period = rstrpos( image_name, '.' )
filestem = strmid( image_name, 0, period )
f = findfile( filestem+'.rpj*' )
if not keyword_set(f) then begin
  print, 'No saved reprojections found for filestem '+filestem
endif else begin
  nf = n_elements(f)
  if nf eq 1 then savefile = f[0] else begin
    if keyword_set(reprojnum) then begin
      savefile = f[reprojnum-1]
      reprojnum = 0
    endif else begin
      for j=0,nf-1 do print, j+1, '    ', f[j]
      reply = ''
      while reply eq '' do begin
        print, 'Multiple saved reprojections detected.  Please select a number [1,' + $
               strtrim(nf,2) + '] '
        read, reply
        reply = fix(reply)
        if reply lt 1 or reply gt nf then reply=''
      endwhile
      savefile = f[reply-1]
    endelse
  endelse
  print, 'Restoring reprojection information from '+savefile
  restore, savefile
  if keyword_set(rpj_rads) then begin
    mnrad = rpj_rads[0]
    mxrad = rpj_rads[1]
    mnlon = rpj_lons[0]
    mxlon = rpj_lons[1]
  endif
  print, 'Radii      (mnrad,mxrad) :  ',strtrim([mnrad,mxrad],2)
  print, 'Longitudes (mnlon,mxlon) :  ',strtrim([mnlon,mxlon],2)
  reproj_mnrad=mnrad & reproj_mxrad=mxrad
  reproj_mnlon=mnlon & reproj_mxlon=mxlon
  rpj_sz = size(rpi)
  print, 'Image dimensions:  ',strtrim(rpj_sz[1:2],2)

  ; gilthoniel's screen size is 1920x1200
  ; home's screen size is 1280x960
  if not keyword_set(screenx) then screenx = 1890;1250
  if not keyword_set(screeny) then screeny = 1000;800
  if rpj_sz[1] gt screenx or rpj_sz[2] gt screeny then begin
    @display_reproj
  endif else begin
    window, 3, xs=rpj_sz[1], ys=rpj_sz[2]
    tv, rpi
  endelse
endelse

end
