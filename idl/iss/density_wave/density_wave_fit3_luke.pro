pro density_wave_fit3_luke, wfile, pp, _rres1, _sigma1, sfile=sfile

if n_params() eq 0 then begin
  print, 'Syntax:'
  print, 'DENSITY_WAVE_FIT3_LUKE, wfile, pp, rres1, sigma1'
  retall
endif

; Inputs: 
; wfile is the name of the file in which relevant parameters are saved, 
;   it should be created by @density_wave_fit_prepare .
; You can enter the values rres1 and sigma1 that were determined by the 
;   dispersion fitting routine density_wave_fit2_luke .  These will be used
;   as starting values for the downhill-simplex fit.
; sfile is a file in which the radial interval that you select is saved.  If 
;   you are fitting multiple waves, then use a different name for sfile for 
;   each one.  Default sfile = 'density_wave_fit3.sav'

; This common block is the means by which all values (except the fitting 
; parameters p0) are transmitted into fdensity_wave3, which calculates the
; theoretical wave profile.
common fdensity_wave33, which_params, m, lambda, lambdasat, rsat, thoukm, $
        rres, rr, yy, rres1, sigma1

; Whether to fit: amplitude, surface density (dispersion), resonance location,
; phase, mean level
which_params = [ 1, 1, 1, 1, 0, 0 ]

; Restore wave profile, configure some variables.
if not keyword_set(wfile) then wfile = 'test_wave.sav'
restore, wfile
if keyword_set(_rres1) then rres1 = _rres1
if keyword_set(_sigma1) then sigma1 = _sigma1
rr = radi - thoukm
bg = .02916
if keyword_set(rres1) then rres = rres1 else rres1 = rres
if keyword_set(sigma1) then sigma = sigma1 else begin
  sigma = 43.8 * 1e10  ; g/km^2
  sigma1 = sigma
endelse
a = .001

; To approximate an optical depth profile, take the square root of I/F.
yy = sqrt(val)
yy = yy - sqrt(bg)

; Load the array of parameters to fit (p0) with initial guesses.
; Also define scale, which gives the range over which each parameter can vary.
if which_params[0] eq 1 then begin
  p0 = [a]
  scale = [1]
endif else stop, 'Error will occur because p0 is not defined.'
if which_params[1] eq 1 then begin
  p0 = [ p0, sigma ]
  scale = [ scale, 4e11 ]
endif
if which_params[2] eq 1 then begin
  p0 = [ p0, rres ]
  scale = [ scale, 10 ]
endif
if which_params[3] eq 1 then begin
  p0 = [ p0, 0 ]
  scale = [ scale, !pi ]
endif
if which_params[4] eq 1 then begin
  p0 = [ p0, 0 ]
  scale = [ scale, .1 ]
endif

; Set up plotting, and interactively define the radial interval for fitting.
; The chosen interval is plotted in red.  Follow instructions at command line.
sfile = 'density_wave_fit3.sav'
if keyword_set(findfile(sfile)) then restore, sfile
xtit = 'Radius - '+strtrim(long(thoukm),2)+' km'
ytit = 'I/F'
if not keyword_set(tit) then tit=''
mb = 1
if keyword_set(x1) then begin
  plot, rr, yy, /xs, /ys, xtit=xtit, ytit=ytit, tit=tit
  oplot, rr[[x1,x2]], yy[[x1,x2]], ps=4, co=ctred()
  oplot, rr[x1:x2], yy[x1:x2], co=ctred()
  print, 'Previously saved radial interval:  [ '+$
	strtrim(rr[x1],2)+' , '+strtrim(rr[x2],2)+' ].'
  print, 'Right-click if you don''t like this interval and want to define a new one.'
  print, 'Left-click to proceed using this interval.'
  cursor, x, y, 4
  mb = !err
endif
while mb eq 4 do begin
  plot, rr, yy, /xs, /ys, xtit=xtit, ytit=ytit, tit=tit
  print, 'Click to determine the minimum radius.'
  cursor, x, y, 4, /data
  x1 = max(where( rr lt x ))
  oplot, [rr[x1]], [yy[x1]], ps=4, co=ctred()
  print, 'Click to determine the maximum radius.'
  cursor, x, y, 4, /data
  x2 = max(where( rr lt x )) + 1
  oplot, [rr[x2]], [yy[x2]], ps=4, co=ctred()
  oplot, rr[x1:x2], yy[x1:x2], co=ctred()
  print, 'You have selected:  [ '+$
	strtrim(rr[x1],2)+' , '+strtrim(rr[x2],2)+' ].'
  print, 'Right-click if you want to redo this interval, left-click to continue.'
  cursor, x, y, 4
  mb = !err
endwhile
save, x1, x2, filename=sfile

; Crop rr and yy to just the chosen interval, saving _rr and _yy in case you
; need them.  Remember, rr and yy are fed into fdensity_wave3 via common block,
; so they need to be in their proper configuration by this time.
_rr = rr & _yy = yy
rr = rr[x1:x2]
yy = yy[x1:x2]

; Feed everything into amoeba, the downhill-simplex fitting module.
print, 'Fitting density wave with downhill-simplex algorithm...'
pp = amoeba( 1e-5, scale=scale, p0=p0, function_value=fval, $
	function_name='fdensity_wave3' )

; Plot the fitted profile onto the data in green.
foo = fdensity_wave3( pp, f=f )
oplot, rr, f, co=ctgreen()
print, pp
stop

end

