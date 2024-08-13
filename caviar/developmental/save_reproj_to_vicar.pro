pro save_reproj_to_vicar, filestem, num

if not keyword_set(num) then num = '1'

restore, filestem + '.rpj' + num

savefile = filestem + '_rpj' + num + '.IMG'

if keyword_set(findfile(savefile)) and not keyword_set(overwrite) then stop, $
	savefile+' already exists.  To overwrite, set overwrite=1 .'
overwrite = 0

print, 'Saving reprojected image in '+savefile
write_vicar, savefile, rrpi

end



