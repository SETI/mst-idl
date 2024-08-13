pro density_wave_fit2_luke, wfile, peaks, rts, aa, aa_sigma, rres1, sigma1, $
        sm=sm, sfile=sfile

if n_params() eq 0 then begin
  print, 'Syntax:'
  print, 'DENSITY_WAVE_FIT2_LUKE, wfile, peaks, rts, aa, aa_sigma, rres1, sigma1'
  retall
endif

; Inputs: 
; wfile is the name of the file in which relevant parameters are saved, 
;   it should be created by @density_wave_fit_prepare .
; sfile is a file in which the rts points that you select are saved.  If you 
;   are fitting multiple waves, then use a different name for sfile for each 
;   one.  Default sfile = 'density_wave_fit2.sav'
; 
; The routine will prompt you to select which half-cycles to fit with sine 
; waves.  We'll call the number of half-cycles n.
; 
; Outputs: 
; peaks is an n-element vector containing the peak locations.
; rts is an (n+1)-element vector containing the locations of the "roots" that
;   define the boundaries of the half-cycles.  These are determined through an
;   interactive process (and are saved to a file from which they can be 
;   restored)
; aa is a 4xn array containing the midlevel, amplitude, wavenumber, and phase
;   for each half-cycle.
; aa_sigma is a 4xn array containing the uncertainties for each element in aa.
; sigma1 is the fitted surface density.
; rres1 is the fitted resonance location.

if not keyword_set(wfile) then wfile = 'test_wave.sav'
restore, wfile

; Prepare variables for plotting
if not keyword_set(_radi) then _radi = radi
if not keyword_set(thoukm) then thoukm = min(_radi) - (min(_radi) mod 1000)
radi = _radi - thoukm
xtit = 'Radius - '+strtrim(long(thoukm),2)+' km'
ytit = 'I/F'
nn = n_elements(radi)
if not keyword_set(tit) then tit=''
if not keyword_set(sfile) then sfile = 'density_wave_fit2.sav'
if not keyword_set(sm) then sm = 150

; Define a smoothed profile bb, and use it to determine the boundaries of the 
; half-cycles (rts).  Default smoothing parameter is 150, but can be specified 
; with input keyword sm.  This is not a precise method, but it is used solely 
; for the determination of the boundaries of half-cycles, so the accuracy is 
; sufficient.
redosm:
bb = smooth([ replicate(val[0],sm), val, replicate(val[nn-1],sm) ],sm)
bb = bb[sm:sm+nn-1]
rts = where( sign((val-bb)[1:nn-1]) ne sign((val-bb)[0:nn-2]), rtscount )
select = replicate( 0, rtscount )
!err = 0

; Plot the data profile in white, the smoothed profile in cyan, with the 
; half-cycle boundaries as yellow diamonds.  When you click on a diamond, it 
; turns orange, and selected intervals are alternating green and purple.
while !err ne 4 do begin
  plot, radi, val, /xs, /ys, xtit=xtit, ytit=ytit, tit=tit
  oplot, radi, bb, co=ctcyan()
  if rtscount gt 0 then begin
    foo = where( select eq 1, count )
    if count gt 1 then for j=0,count-2 do begin
      if j mod 2 eq 0 then clr=ctgreen() else clr=ctpurple()
      oplot, radi[rts[foo[j]]:rts[foo[j+1]]], val[rts[foo[j]]:rts[foo[j+1]]], $
	co=clr
    endfor
    if count ne 0 then oplot, radi[rts[foo]], val[rts[foo]], ps=4, co=ctorange()
    foo = where( select eq 0, count )
    if count ne 0 then oplot, radi[rts[foo]], val[rts[foo]], ps=4, co=ctyellow()
  endif
  print, 'Click on points to select, click again to de-select.'
  print, 'Click to the left of the plot to change the smoothing parameter (current='+strtrim(sm,2)+').'
  print, 'Click to the right of the plot to restore a previous array of selected points.'
  print, 'Click under the plot to quit.'
  print, 'Right-click to proceed with wave fitting.'
  cursor, x, y, 3, /data
  if y lt min(val) then begin
    retall
  endif else if x lt min(radi) then begin
    print, 'Enter new smoothing parameter.'
    read, reply1
    sm[0] = reply1
    goto, redosm
  endif else if x gt max(radi) then begin
    _select = select
    if keyword_set(findfile(sfile)) then restore, sfile else begin
      print, sfile + ' not found.'
    endelse
    if n_elements(select) ne rtscount then begin
      print, 'Rejecting restored selected points array because it has different dimensions.'
      select = _select
    endif else if (where(select ne _select))[0] eq -1 then begin
      print, 'No change to selected points array.'
    endif else begin
      print, 'Restored selected points array.'
    endelse
  endif else if !err ne 4 then begin
    dist = sqrt( (radi[rts]-x)^2 + (val[rts]-y)^2 )
    cc = (where( dist eq min(dist), count ))[0]
    select[cc] = 1 - select[cc]
  endif
endwhile

oplot, radi, val
foo = where( select eq 1, rtscount )
if rtscount gt 1 then rts = rts[foo] else begin
  print, ([ 'No points', 'Only 1 point' ])[rtscount] + ' selected.'
  retall
endelse
save, select, filename=sfile
aa = fltarr(4,rtscount-1)
aa_sigma = fltarr(4,rtscount-1)
peaks = fltarr(rtscount-1)
for j=0,rtscount-2 do begin

  ; Define the interval for this half-cycle, also the intervals for the 
  ; previous cycle and the next one.
  ni = rts[j+1] - rts[j] + 3
  int = indgen(ni) + rts[j]
  if j ne 0 then lastint = indgen( rts[j]-rts[j-1] ) + rts[j-1]
  if j ne rtscount-2 then nextint = indgen( rts[j+2]-rts[j+1] ) + rts[j+1]
  r = radi[int]
  v = val[int]

  ; If this half-cycle is a local maximum (concave down), then cyc=1 else cyc=0
  ; Check that this cycle is in the opposite sense from the last one.
  if j ne 0 then _cyc = cyc
  mn = (where( v eq min(v) ))[0]
  mx = (where( v eq max(v) ))[0]
  md = ni / 2
  cyc = (where( abs([mn,mx]-md) eq min(abs([mn,mx]-md)) ))[0]
  if j ne 0 then if cyc eq _cyc then begin
    oplot, r[lastint], v[lastint], co=ctgreen()
    oplot, r[int], v[int], co=ctpurple()
    up = [ 'local minima (concave up).', 'local maxima (concave down).' ]
    print, 'Both this half-cycle and the last half-cycle appear to be '+up[cyc]
    stop
  endif

  ; Prepare variables to be entered into curvefit.
  _aa = fltarr(4)
  if j eq 0 then lastint = nextint
  if cyc eq 0 then begin
    mxint = nextint
    mnint = int
    ext = min(val[int])
  endif else begin
    mxint = int
    mnint = nextint
    ext = max(val[int])
  endelse
  ; Initial guesses for aa.
  ; The four fitted parameters are midlevel, amplitude, wavenumber, and phase.
  _aa[0] = ( max(val[mxint]) + min(val[mnint]) )/2
  _aa[1] = ext - _aa[0]
  _aa[2] = !pi / ( radi[rts[j+1]] - radi[rts[j]] )
  _aa[3] = - _aa[2] * radi[rts[j]]
  weights = replicate( 1., n_elements(int) )
  ; Fit data to   f = a[0] + a[1] * sin( a[2]*x + a[3] )
  ; This function is contained in fsine.pro, which is called by curvefit.
  yfit = curvefit( r, v, weights, _aa, _aa_sigma, function_name='fsine' )

  ; Plot fitted sine wave over the data in green.
  oplot, r, yfit, co=ctgreen()
  ; Enter results into arrays aa and aa_sigma.
  aa[*,j] = _aa
  aa_sigma[*,j] = _aa_sigma
  ; Fit peak location for this half-cycle.
  theta = ( _aa[2]*r + _aa[3] )*180/!dpi mod 360
  foo = where( theta lt 0, count )
  if count gt 0 then theta[foo] = theta[foo] + 360
  if theta[n_elements(theta)/2] lt 180 then begin
    pk = 90 
    foo = where( theta gt 270, count )
    if count gt 0 then begin
      theta = vec_remove( theta, foo )
      r = vec_remove( r, foo )
      yfit = vec_remove( yfit, foo )
    endif
  endif else begin
    pk = 270
    foo = where( theta lt 90, count )
    if count gt 0 then begin
      theta = vec_remove( theta, foo )
      r = vec_remove( r, foo )
      yfit = vec_remove( yfit, foo )
    endif
  endelse
  peaks[j] = interpol( r, theta, pk )
  ; Plot a vertical green dotted line from the peak to the [bottom,top] of the 
  ; plot for a half-cycle that is concave [down,up].
  oplot, peaks[j]*[1,1], [ interpol(yfit,r,peaks[j]), (1-cyc*2)*1e3 ], $
	co=ctgreen(), l=1

endfor

; Use the peak locations to define the wavenumber at each peak.  Plot.
; Do a linear fit to determine resonance location and surface density.
npk = n_elements(peaks)
kk = fltarr(npk)
for j=1,npk-2 do kk[j] = 2*!pi/( peaks[j+1] - peaks[j-1] )
ppeaks = peaks[1:npk-2]
kk = kk[1:npk-2]
ffit = poly_fit(ppeaks,kk,1)
window, /free
plot, ppeaks, kk, ps=4, /xs, /ys, xtit=xtit, ytit='Wavenumber (km^-1)'
oplot, radi, poly( radi, ffit )
rres1 = -ffit[0]/ffit[1] + thoukm
; Rosen et al (1991), equation 13
sigma1 = 2*!pi/3.08 * (m-1) / (rres1/60330)^4 / ffit[1] * 1e10

end


