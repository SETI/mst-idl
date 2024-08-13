pro displayimages, dir=dir, suffix=suffix, files=files

if not keyword_set(dir) then dir = '' else begin
  if strmid(dir,strlen(dir)-1,1) ne '/' then dir = dir + '/'
endelse
if not keyword_set(suffix) then suffix = '.IMG'
if not keyword_set(files) then files = findfile(dir+'*'+suffix)

nf = n_elements(files)
; We assume a window size of 640x512 and an image size that can be scaled
; to 128x128 (ISS images are generally 1024x1024)
nps = 20
imsz = 128
nscreens = nf/nps + (nf mod nps)/(nf mod nps)
for j=0,nscreens-1 do begin
  print, 'Reading from left to right, then top to bottom:'
  j0 = j*nps
  j1 = ((j+1)*nps)<nf - 1
  window
  for k=j0,j1 do begin
    im = read_vicar( files[k], /silent )
    im1 = rebin( im, imsz, imsz )
    tvscl, im1, k mod nps, /order
    print, files[k]
  endfor
  print, 'Images '+strtrim(j0+1,2)+' through '+strtrim(j1+1,2)+$
	', of '+strtrim(nf,2)
  reply = ''
  if j1 ne (nf-1) then read, reply
endfor

end
