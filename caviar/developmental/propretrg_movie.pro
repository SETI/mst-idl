; Create files with extension .bksub80 by doing the following:
; restore, 'stretch.sav'
; norings = 1
; nostars = 1
; nocray = 1
; fadefac = 0.8
; 
; jjj=jjj+1 & print,jjj & image_name=filenames[jjj]
; @caviar
; @ns_radscan
; @save_bksub
; 
; The image that shows when ns_radscan completes will be stretched badly,
; but that's okay. 

; These are the values for 247
; For 234, the values are jstart = 60 and jend = 72
; For 240, the values are jstart = 80 and jend = 88
if not keyword_exists(jstart) then jstart = 91
if not keyword_exists(jend) then jend = 96

restore, 'stretch.sav'
norings = 1
nostars = 1
nocray = 1
if not keyword_exists(bksub) then bksub = 1
if not keyword_exists(fadefac) then fadefac = 0.8
jjj = jstart - 1
next:
jjj = jjj + 1
image_name=filenames[jjj]
@caviar
if keyword_set(bksub) then file=bksubfile else file=strmid(image_name,0,17)
tifffile = file + '.tiff'
write_tiff, tifffile, im
if jjj lt jend then goto, next

end
