restore, 'stretch.sav'
if not keyword_exists(jjj) then jjj = -1
noplot = 1
cray_zero = 1
jjj=jjj+1 & image_name=filenames[jjj]
@caviar
.run cray_auto
print, strtrim(jjj,2) + ' / ' + strtrim(n_elements(filenames),2)
@ns_radscan
if n_elements(cray) gt 1 then bksub_img[cray[*,1],cray[*,0]] = 0
@save_bksub
outim = bksub_img>(-.01)<.01
outim = (outim-min(outim))*255/(max(outim)-min(outim))
outim = byte(outim)
write_tiff, image_name+'.bksub.tiff', outim
