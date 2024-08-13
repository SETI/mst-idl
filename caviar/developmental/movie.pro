if not keyword_set(filenames) then restore, 'stretch.sav'
if not keyword_set(movie_min) then print, 'Set movie_min= to the min stretch value.'
if not keyword_set(movie_min) then retall
if not keyword_set(movie_max) then print, 'Set movie_max= to the max stretch value.'
if not keyword_set(movie_max) then retall
for j=0,n_elements(filenames)-1 do begin &$
  im = read_vicar(filenames[j]) &$
  tvscl, /order, im>movie_min<movie_max &$
endfor
