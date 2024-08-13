; Fix stretch.sav when files have been updated with new versions.
restore, 'stretch.sav'
n1 = n_elements(filenames)
.run update_stretch
n2 = n_elements(filenames)
xx = lindgen(n2-n1) + n1
yy = lonarr(n2-n1)
spawn, 'mkdir prelim'
for j=0,nadd-1 do begin &$
  yy[j] = (where( strmid(filenames[xx[j]],0,11) eq $
                  strmid(filenames,0,11), count ))[0] &$
  if count ne 2 then print, 'Alert '+strtrim(j,2) &$
  spawn, 'mv '+strmid(filenames[yy[j]],0,13)+'* prelim' &$
endfor
params = [ 'filenames', 'lmax', 'lmin', 'rmax', 'rmin', $
           'stretchmax', 'stretchmin', '_keywords' ]
nparams = n_elements(params)
; filenames[yy] = filenames[xx]
for j=0,nparams-1 do foo = execute( params[j]+'[yy] = '+params[j]+'[xx]' )
; filenames = filenames[0:n1-1]
for j=0,nparams-1 do foo = execute( params[j]+' = '+params[j]+'[0:n1-1]' )
; Save as in update_stretch.pro
if keyword_set(_nostars) then $
  save, filenames, lmax, lmin, nostars, ring_npoints, rmax, rmin, $
        stretchmax, stretchmin, usehistogram, diffuse, basiconly, $
        order1, order2, fewerstars, fullhist, _keywords, $
        filename='stretch.sav'
if not keyword_set(_nostars) then $
  save, filenames, lmax, lmin, ring_npoints, rmax, rmin, $
        stretchmax, stretchmin, usehistogram, diffuse, basiconly, $
        order1, order2, fewerstars, fullhist, _keywords, $
        filename='stretch.sav'
