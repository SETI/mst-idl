if not keyword_set(filenames) then restore, 'stretch.sav'
window, 0, xs=1024, ys=1024
if keyword_set(double) then begin
  window, 2, xs=1024, ys=1024
  wset, 0
endif
;if not keyword_set(movie_min) then print, 'Set movie_min= to the min stretch value.'
;if not keyword_set(movie_min) then retall
;if not keyword_set(movie_max) then print, 'Set movie_max= to the max stretch value.'
;if not keyword_set(movie_max) then retall
if keyword_set(movie_filenums) then begin
  movie_filenames=filenames[movie_filenums] 
endif else begin
  movie_filenames=filenames
  movie_filenums=indgen(n_elements(filenames))
endelse
unget_color
if not keyword_set(movie_increment) then movie_increment = 1
if keyword_exists(movie_min) then begin
  if n_elements(movie_min) eq 1 then begin
    movie_min = replicate(movie_min,n_elements(filenames))
  endif 
endif
if keyword_exists(movie_max) then begin
  if n_elements(movie_max) eq 1 then begin
    movie_max = replicate(movie_max,n_elements(filenames))
  endif 
endif
for j=0,n_elements(movie_filenames)-1,movie_increment do begin
  print, strtrim(j,2)+' ('+strtrim(movie_filenums[j],2)+') / '+$
         strtrim(n_elements(movie_filenames),2)+'     '+movie_filenames[j]
  if keyword_set(bksub) then begin
    lastperiod = rstrpos( movie_filenames[j], '.' )
    bksub_file = strmid(movie_filenames[j],0,lastperiod) + '.bksub'
    restore, bksub_file
    stmin = -2*stddev(bksub_img)
    stmax = 2*stddev(bksub_img)
    im = bksub_img > stmin < stmax
    if keyword_set(fadefac) then begin
      if fadefac gt 0 and fadefac lt 1 then begin
        bksub_file = bksub_file + string(fadefac*100,fo='(I2)')
        stmin = stmin*(1-fadefac)
        stmax = stmax*(1-fadefac)
        restore, bksub_file
        im = bksub_img
      endif 
    endif 
  endif else begin
    im = read_vicar(movie_filenames[j])
  endelse
  if keyword_set(hipass) then begin
    im = im - smooth(im,10)
    if keyword_exists(movie_max) then begin
      stmax=movie_max[movie_filenums[j]]
    endif else stmax=.01
    stmin = -stmax
  endif else begin
    run_histogram, im, stmin, stmax, fail=fail, fullhist=fullhist, $
                   threshold=movie_threshold
    if keyword_exists(movie_min) then stmin=movie_min[movie_filenums[j]]
    if keyword_exists(movie_max) then stmax=movie_max[movie_filenums[j]]
    print, 'Brightness levels: '+string(stmin)+string(stmax)
  endelse 
  if keyword_set(movie_gamma) then gamma_ct, movie_gamma
;  if keyword_set(movie_ct) then loadct, ct
  tvscl, /order, im>stmin<stmax
  if keyword_set(immax) then begin
    ; This is specially meant to check diffring_exposures.pro
    nl = (size(im))[1]
    x = float(nl)/512
    plots, immax[0,movie_filenums[j]], nl-1-immax[1,movie_filenums[j]], $
           /device, ps=3, color=red()
    plots, immax[0,movie_filenums[j]] + [-1,1,1,-1,-1]*3*x, /device, $
           nl-1-immax[1,movie_filenums[j]] + [-1,-1,1,1,-1]*3*x, color=red()
  endif
  if keyword_set(movie_wait) then begin
    if movie_wait eq -1 then stop else wait, movie_wait
  endif
  if keyword_set(double) then begin
    wset, 2
    tvscl, /order, im>stmin<stmax
    wset, 0
  endif 
endfor

end
