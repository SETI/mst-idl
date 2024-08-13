if keyword_set(bksubfile) then bksubinsert = '.bksub' else bksubinsert = ''
if not keyword_set(tifffile) then tifffile = image_name+bksubinsert+'.tiff'
outim = im
write_tiff, tifffile, outim

