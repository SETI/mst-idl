if not keyword_exists(jfw) then jfw = 0
.run preparewaveletplots

; Optional switches
; Use previously measured values.
if not keyword_exists(usefp) and keyword_set(review) then usefp = 2
if not keyword_exists(usefp) and not keyword_set(review) then usefp = -1
; Do the quadratic fit, but don't fit a and xi_d with mpcurvefit
if not keyword_exists(novm) then novm = 1   
; Image to use
if keyword_set(review) or usefp ne -1 then restore, 'fitparams.sav'
if keyword_set(review) or usefp ne -1 then j1 = _fitparams[jfw].j1
if not keyword_exists(j1) then j1 = 25
j2 = j1
if not keyword_exists(nosave) then nosave = 0
help, j1, usefp, novm, nosave
print, 'Set usefp=-1 do definitely start a new wave, usefp=1 to use previously'
print, '   measured quantities except phase interval, usefp=2 to use all.'
print, 'Set novm=1 to suppress mpcurvefit (get quadratic fit right first).'
print, 'Set j1 to specify the image.'
print, 'Set nosave=1 if you don''t want fit parameters saved in fitparams.sav'

if keyword_set(dolzr) then fwdolzr = 1 else fwdolzr = 0
dolzr = 0
.run makewaveletplots
if keyword_set(fwdolzr) then dolzr = 1
fwdolzr = 0
.run fit_wavelet1

if keyword_set(review) then jfw = jfw + 1
