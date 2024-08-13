if not keyword_set(filenames) then restore, 'stretch.sav'
nf = n_elements(filenames)
locations_array = fltarr(1000,nf)
hist_array = lonarr(1000,nf)
for j=0,nf-1 do begin
  print, strtrim(j,2)+' / '+strtrim(nf,2)+'     '+filenames[j]
  im = read_vicar(filenames[j])
  run_histogram, im, stmin, stmax, locations=locations, hist=hist
  locations_array[*,j] = locations
  hist_array[*,j] = hist
endfor

save, locations_array, hist_array, filename='histogram_array.sav'

end
