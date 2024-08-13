pro run_histogram, rawim, stmin, stmax, locations=locations, many=many, fail=fail, threshold=_threshold, fullhist=fullhist, doplot=doplot, hist=hist, nocrop=nocrop, silent=silent, debug=debug, uselog=uselog

  if n_params() eq 0 then begin
    print, 'Syntax:  RUN_HISTOGRAM, rawim, stmin, stmax, locations=, hist='
    retall
  endif
  if not keyword_exists(uselog) then begin
    if stddev(rawim) lt mean(rawim) then uselog = 1
  endif

  fail = 0
  nl = (size(rawim))[1]
  if not keyword_set(nocrop) then begin
    _rawim = rawim[2:nl-3,2:nl-3]  ; Crop out the borders for histogram purposes
  endif else _rawim = rawim
  if keyword_set(uselog) then begin
    foo = where( _rawim le 0, count )
    if count gt 0 then _rawim[foo] = 1e-8
    _rawim = alog10(_rawim)
  endif
  if (size(rawim))[3] le 3 then nbins=0 else nbins=1000
  hist = histogram(_rawim,nbins=nbins,locations=locations)
  if keyword_set(uselog) then locations = 10^locations
  hist = smooth( hist, 20, /edge_truncate )
  ;threshold = float(nl)^2/4000
  if keyword_exists(_threshold) then threshold=_threshold else threshold = 20
  many = 1 + where( hist[1:n_elements(hist)-2] gt threshold, count )
  if count ge 2 then begin
    if not keyword_set(silent) then begin
      print, 'Brightness information automatically determined.'
    endif
    mkexed, many, exed, z
    if z gt 0 and not keyword_set(fullhist) then begin
      ; If there are multiple peaks
;      if (where(rawim eq 0))[0] ne -1 and exed[0,0] eq 1 then begin
;        ; Exclude a peak that includes zero
;        exed = exed[*,1:z]
;        z = z - 1
;      endif
      ; Choose the widest peak in the histogram
      nexed = locations[exed[1,*]] - locations[exed[0,*]]
      foo = (where( nexed eq max(nexed) ))[0]
      many = many[where( many ge exed[0,foo] and many le exed[1,foo] )]
    endif
    stmin = locations[min(many)]
    stmax = locations[max(many)+1]
  endif else fail = 1
  if keyword_set(doplot) then begin
    plot, locations, hist, ps=10
  endif
  if keyword_set(debug) then stop

end
