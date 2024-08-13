if not keyword_exists(filenames) then restore, 'stretch.sav'
if not keyword_set(nf) then nf = n_elements(filenames)
if not keyword_exists(jjj) then jjj = -1
jjj = jjj + 1 ;cancel out if not recursive
image_name = filenames[jjj]
print, '----------'
print, strtrim(jjj,2)+' / '+strtrim(n_elements(filenames),2)+'   '+image_name
print, '----------'
noplot = 1
@caviar
noplot = 0
if keyword_set(zscale) and not keyword_set(width) then restore, '../rhea_reproject_ringwidth.sav'
if keyword_set(zscale) then fac = width[jjj]/10
.run rhea_reproject_b
tvscl, imrz>(-stddev(imrz))<stddev(imrz)/4
sz = size(imrz)
if not keyword_set(__rr) then __rr = fltarr( sz[1], nf )
if not keyword_set(__imrz) then __imrz = fltarr( sz[1], sz[2], nf )
__rr[*,jjj] = rr
__imrz[*,*,jjj] = imrz

save, __rr, __imrz, nf, dr, rmin, rmax, filenames, filename='rhea_reproject.sav'
