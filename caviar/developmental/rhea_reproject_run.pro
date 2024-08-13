if not keyword_exists(filenames) then restore, 'stretch.sav'
if not keyword_set(nf) then nf = n_elements(filenames)
if not keyword_exists(jjj) then jjj = -1
jjj = jjj + 1
image_name = filenames[jjj]
print, '----------'
print, strtrim(jjj,2)+' / '+strtrim(n_elements(filenames),2)+'   '+image_name
print, '----------'
@caviar
.run rhea_reproject
tvscl, imrz>(-stddev(imrz))<stddev(imrz)
sz = size(imrz)
if not keyword_set(__rr) then __rr = fltarr( sz[1], nf )
if not keyword_set(__imrz) then __imrz = fltarr( sz[1], sz[2], nf )
__rr[*,jjj] = rr
__imrz[*,*,jjj] = imrz

save, __rr, __imrz, nf, dr, rmin, rmax, filenames, filename='rhea_reproject.sav'
