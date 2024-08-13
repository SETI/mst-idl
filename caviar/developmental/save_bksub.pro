period = rstrpos( image_name, '.' )
suffix = strmid( image_name, period, strlen(image_name) )
if suffix ne '.IMG' then print, 'Image_name suffix is '+suffix+' not .IMG'
savefile = strmid( image_name, 0, period ) + '.bksub'
if keyword_set(fadefac) then savefile = savefile+string(fadefac*100,fo='(I2)')
if keyword_set(findfile(savefile)) and not keyword_set(overwrite) then stop, $
	savefile+' already exists.  To overwrite, set overwrite=1 .'
overwrite = 0

print, 'Saving bksub image in '+savefile
save, cmat, bksub_descrip, radial_filter_median, bksub_img, filename=savefile

