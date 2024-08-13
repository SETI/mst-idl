pro run_calculate_exposures, exposure, frac, phase=phase, ringfrac=ringfrac, $
  bgfrac=bgfrac, filtername=filtername, jj=jj, $
  noplot=noplot, gain=gain, sum=sum, elevation=elevation, debug=debug

if keyword_set(frac) then begin
  nn = n_elements(frac)
  exposure = fltarr(nn)
endif else begin
  nn = n_elements(exposure)
  frac = fltarr(nn)
endelse

vars = [ 'phase', 'elevation', 'ringfrac', 'bgfrac', 'filtername', 'jj', $
         'gain', 'sum' ]
for j=0,n_elements(vars)-1 do foo = execute( $
  'if keyword_set('+vars[j]+') then begin &' + $
  '  if n_elements('+vars[j]+') eq 1 then ' + $
                           vars[j]+' = replicate( '+vars[j]+', nn ) &' + $
  '  if n_elements('+vars[j]+') ne nn then stop &' + $
  'endif else '+vars[j]+' = replicate( 0., nn )' )

for k=0,nn-1 do begin
  print, strmid(k,2)+' / '+strmid(nn,2)+' :  '
  print, exposure[k], frac[k], phase[k], elevation[k], '    '+filtername[k], $
         jj[k], gain[k], sum[k]
  _exposure = exposure[k] & _frac = frac[k]
  calculate_exposures, _exposure, _frac, phase=phase[k], $
                       ringfrac=_ringfrac, bgfrac=_bgfrac, $
                       filtername=filtername[k], jj=jj[k], noplot=noplot, $
                       gain=gain[k], sum=sum[k], elevation=elevation[k]
  exposure[k] = _exposure & frac[k] = _frac
  ringfrac[k] = _ringfrac & bgfrac[k] = _bgfrac
  if keyword_exists(noplot) then if noplot eq 0 then begin
    if !d.name eq 'X' and !p.multi[0] eq 0 then stop
  endif
endfor

if keyword_set(debug) then stop

end

