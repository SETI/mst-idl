function average_darks, filenames, savefile

nf = n_elements(filenames)
_im = read_vicar( filenames[0], label )
sz = size(_im)
if sz[sz[0]+1] eq 4 then im = fltarr( sz[1], sz[2], nf ) else $
  if sz[sz[0]+1] eq 2 then im = intarr( sz[1], sz[2], nf ) else stop
im[*,*,0] = _im
for j=1,nf-1 do begin
  _im = read_vicar( filenames[j] )
  im[*,*,j] = _im
endfor
;out = median( im, dimension=3 )
out = fltarr( sz[1], sz[2] )
if not keyword_set(sigmathresh) then sigmathresh = 5
for j=0,sz[1]-1 do for k=0,sz[2]-1 do begin
  use = indgen(nf)
  fail = 0
  while stddev(im[j,k,use]) gt sigmathresh and not keyword_set(fail) do begin
    diff = abs( im[j,k,use] - median(im[j,k,use]) )
    use = vec_remove( use, (where( diff eq max(diff) ))[0] )
    if n_elements(use) eq 2 then begin
      fail = 1
      print, 'Failed to find pixels with small variance: ',j,k
    endif
  endwhile
  if keyword_set(fail) then begin
    out[j,k] = median( im[j,k,*] )
  endif else begin
    out[j,k] = mean( im[j,k,use] )
  endelse
endfor

if keyword_set(savefile) then begin
  if keyword_set(findfile(savefile)) then begin
    reply = ''
    while reply eq '' do begin
      print, savefile+' exists.  Continue? (y/n)'
      read, reply
      if reply eq 'n' then retall else if reply ne 'y' then reply=''
    endwhile
  endif
  write_vicar, savefile, out, label
endif

return, out

end
