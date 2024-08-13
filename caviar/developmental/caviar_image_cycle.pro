if not keyword_set(findfile('stretch.sav')) then stop, 'stretch.sav not found; run update_stretch first.'
if not keyword_set(filenames) then restore, 'stretch.sav'
if not keyword_exists(jjj) then jjj = -1
jjj = jjj + 1
nf = n_elements(filenames)
if jjj ge nf then stop, 'No more images'
image_name = filenames[jjj]
print, 'Opening '+image_name+' ('+strtrim(jjj,2)+'/'+strtrim(nf,2)+')'
@caviar

