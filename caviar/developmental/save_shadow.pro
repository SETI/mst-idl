period = rstrpos( image_name, '.' )
suffix = strmid( image_name, period, strlen(image_name) )
if suffix ne '.IMG' then print, 'Image_name suffix is '+suffix+' not .IMG'
savefile = strmid( image_name, 0, period )+'.shadow'
if keyword_set(findfile(savefile)) and not keyword_set(overwrite) then stop, $
	savefile+' already exists.  To overwrite, set overwrite=1 .'
overwrite = 0

print, 'Saving shadow information in '+savefile
save, shadow, filename=savefile

end
