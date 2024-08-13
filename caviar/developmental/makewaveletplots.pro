;.run preparewaveletplots
restore, 'stretch.sav'

; Initialize variables.
mooncolor = 1
radscan_keepwin = 1
equal = 0
specify_levels = 0
if not keyword_exists(j1) then j1 = 0
if not keyword_exists(j2) then j2 = nj-1
if not keyword_set(imnum) then imnum = indgen(nj)
if not keyword_set(nwakes) then nwakes = replicate(0,nj)
;aspect = .4

for jj=j1,j2 do begin

  ; Load variables for this image.
  jimg = imnum[jj]
  polera=40.58756d0
  poledec=83.53684d0
  if not keyword_set(_et) then restore, 'et.sav'
  et = _et[jimg]
  if keyword_set(xxr) then wavelet_xr = xxr[*,jj]
;  if !d.name eq 'X' then window;, xs=1900, ys=1000
  if keyword_set(_notres) then notres = *(_notres[jimg])
  minlam = _minlam[jimg]
  maxlam = _maxlam[jimg]
  top = _top[jimg]
  sigma = _sigma[jimg]
  dr = _dr[jimg]
  @reset_radscan
  ring_rads = 0 & ring_rads_legend = ''
  _rmin = rmin[jimg] & _rmax = rmax[jimg]
  image_name = filenames[jimg]
  print, strtrim(jimg,2)+'    '+image_name
  if keyword_set(nomwptit) then mwptit='' else begin
    if not keyword_set(obs) then obs=''
    mwptit = ' -- '+obs+'('+strtrim(jimg,2)+')'
  endelse
  wavelet_tit=strmid(image_name,0,11)+mwptit
  savefile=strmid(filenames[jimg],0,strpos(filenames[jimg],'.'))+'.scan1'

  ; Retrieve radial scan information
  restore, savefile
  radi = radi + dr
  @restore_ring_rads1
  if keyword_set(_rradd) then if keyword_set(*(_rradd[jimg,0])) then begin
    for j=0,n_elements(*(_rradd1[jimg,0]))-1 do begin
      ring_rads = [ ring_rads, *(_rradd[jimg,0]) ]
      ring_rads_legend = [ ring_rads_legend, *(_rradd[jimg,1]) ]
    endfor
  endif
  @plot_radscan1

  ; If flag set, start printing of plot (don't include the radscan).
  if keyword_set(dolzr) then begin
    lzr, psname+strtrim(jimg,2), half=half, aspect=aspect, qland=qland, qport=qport, hland=hland, hport=hport
    plot_color
    @plot_prepare
    if keyword_set(qland) or keyword_set(qport) then wavelet_charsz=1 else wavelet_charsz=0
    specify_levels = 1
    ;mooncolor = 0
    if keyword_set(nw) then begin
;      if nw[jj] gt 1 then !x.omargin=[0,0] else !x.omargin=[0,42]
    endif
  endif

  ; Create the wavelet plot.
  if keyword_set(_nopan) then nopan = _nopan[jimg]
  if keyword_set(_noatlas) then noatlas = _noatlas[jimg]
  if keyword_set(nopan) then begin
    rrl = ring_rads_legend
    colon = strpos(ring_rads_legend,':')
    if nopan eq 5 then begin
      foo = where( strmid(ring_rads_legend,0,3) eq 'Pan' and $
                   strmid(ring_rads_legend,rotate(colon-1,1),1) ne '0' and $
                   strmid(ring_rads_legend,rotate(colon-1,1),1) ne '5', count )
    endif else begin
      foo = where( strmid(ring_rads_legend,0,3) eq 'Pan' and $
                   strmid(ring_rads_legend,rotate(colon-1,1),1) ne '0', count )
    endelse
    if count gt 0 then ring_rads_legend[foo] = 'Pan'
  endif
  if keyword_set(noatlas) then begin
    rrl = ring_rads_legend
    colon = strpos(ring_rads_legend,':')
    if noatlas eq 5 then begin
      foo = where( strmid(ring_rads_legend,0,2) eq 'At' and $
                   strmid(ring_rads_legend,rotate(colon-1,1),1) ne '0' and $
                   strmid(ring_rads_legend,rotate(colon-1,1),1) ne '5', count )
    endif else begin
      foo = where( strmid(ring_rads_legend,0,2) eq 'At' and $
                   strmid(ring_rads_legend,rotate(colon-1,1),1) ne '0', count )
    endelse
    if count gt 0 then ring_rads_legend[foo] = 'At'
  endif
  @run_wavelet1

  ; If flags set, overplot with models for waves and wakes.
  if keyword_set(nopan) then ring_rads_legend = rrl
  if keyword_set(noatlas) then ring_rads_legend = rrl
  if keyword_set(_rradd1) then if keyword_set(*(_rradd1[jimg,0])) then begin
    for j=0,n_elements(*(_rradd1[jimg,0]))-1 do begin
      ring_rads[n_elements(ring_rads)-1-j] = (*(_rradd1[jimg,0]))[j]
      ring_rads_legend[n_elements(ring_rads)-1-j] = (*(_rradd1[jimg,1]))[j]
    endfor
  endif
  plot_wavemodel, ring_rads, ring_rads_legend, sigma, fit=fit, notres=notres, mooncolor=mooncolor
  if nwakes[jimg] gt 0 then begin
    sat = 618l  ;Pan
    kind = 'SPK'
    cspice_ktotal, kind, spkcount
    if spkcount eq 0 or keyword_set(reloadkernels) then cspice_furnsh,getenv("CAVIAR_KERNELS")
    ; Have defined et, polera, and poledec above.
    @get_sat_coords
    ;theta = 342.*!dpi/180
    ;if jimg ge 35 then theta = 2*!dpi - theta
    plot_wakemodel, radi, _keywords[jimg].ringplane_aimpoint_longitude, sat_polar[1], rpan, wakepredict=wakepredict, nwakes=nwakes[jimg], theta=theta
  endif
  if keyword_set(dolzr) then clzr else if not keyword_exists(jfw) then stop
endfor

end
