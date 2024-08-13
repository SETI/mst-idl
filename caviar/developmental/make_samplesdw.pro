; cd $DATA/images/046/RDHRESSCN
; Set q=1 for the first panel, q=2 for the second panel
; For the samplesdw2 plot, set samplesdw = 2 and samplesdw2 = 1
print, ''
print, 'MAKE_SAMPLESDW:  Set jjj and _wavelet_xr to your liking and then first'
print, '@make_wavelet_plots_rri to see how to set xticks and xticki.  Then set '
print, 'xticks and xticki to those numbers (the number of major xticks and the '
print, 'interval between them).  Also set userr to the resonance label(s) you '
print, 'want to display.  Finally, @make_samplesdw. '
print, ''
cmyk = 1
if keyword_set(samplesdw) then begin &$
  if samplesdw eq 1 then jjj = 13 else if samplesdw eq 2 then jjj = 14 &$
endif else samplesdw = 0
keepplotaxis = 1
notit = 1
wavelet_xr = 0
aspect = 0
specifyxy = 0
xticki = 0
xticks = 0
specifyrings = 0
dolzr = 0
@make_wavelet_plots_rri
if samplesdw eq 1 then wavelet_xr = [126.78,126.91] else $
   if samplesdw eq 2 then wavelet_xr = [125.85,126.105]
if keyword_set(_wavelet_xr) then wavelet_xr = _wavelet_xr
frac = ( wavelet_xr[1] - wavelet_xr[0] )/( !x.crange[1] - !x.crange[0] )
xaxislen = 1.9
specifyxy = [ frac*(9.5-xaxislen) + xaxislen, 0.4*9.5 ]
if samplesdw eq 1 then xticki = 0.1 else if samplesdw eq 2 then xticki = 0.1
if keyword_set(_xticki) then xticki = _xticki
if samplesdw eq 1 then xticks = 2 else if samplesdw eq 2 then xticks = 3
if keyword_set(_xticks) then xticks = _xticks
if not keyword_exists(_dolzr) then _dolzr = 1
dolzr = _dolzr
psnameextra = '_samplesdw'
if keyword_set(samplesdw2) then psnameextra = psnameextra + '2'
if keyword_set(psnameiden) then psnameextra = psnameextra + psnameiden
if samplesdw eq 1 then userr = 'Pr 15:13' else $
   if samplesdw eq 2 then userr = 'Pr 7:6'
specifyrings = where( _ring_rads_legend eq userr[0] )
if n_elements(userr) gt 1 then for j=1,n_elements(userr)-1 do $
  specifyrings = [ specifyrings, (where( _ring_rads_legend eq userr[j] ))[0] ]
keepplotaxis = 0
@make_wavelet_plots_rri
samplesdw = 0
;specifyrings = 0
if _dolzr ne 0 then wdelete
if _dolzr ne 0 then wdelete
