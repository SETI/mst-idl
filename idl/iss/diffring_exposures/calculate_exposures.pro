pro calculate_exposures, exposure, frac, phase=phase, ringfrac=ringfrac, $
  bgfrac=bgfrac, filtername=filtername, jj=jj, nobg=nobg, $
  noplot=noplot, gain=gain, sum=sum, elevation=elevation, debug=debug

if n_params() eq 0 then begin
  print, ''
  print, '  Syntax:  CALCULATE_EXPOSURES, exposure, frac, [phase=, elevation=, gain=, sum=, filtername=, jj=, ringfrac=, bgfrac=]'
  print, ''
  print, '  Calculates exposure times for faint rings observations.'
  print, '  If the total saturation fraction (frac) is specified, it is the input, and the corresponding exposure is calculated.  If exposure is specified and frac is not, then the saturation fractions are calculated from the input exposure.  If neither exposure nor frac is specified, then a total saturation fraction of 0.5 is assumed.'
  print, '  Other input keywords include phase (default 90 degrees), elevation (default 1 degree), gain (default 12, could be 29), sum (default 2, could be 1 or 4), camera/ring and filter (filtername specifies by name, jj by index number, default is E Ring CL1/CL2)'
  print, '  Other output keywords include rings saturation fraction (ringfrac), background saturation fraction (bgfrac)'
  print, ''
  retall
endif

dir = '/home/borogove/iss/caviar/developmental/';'~/idl/iss/diffring_exposures/'
restore, dir + 'bgpredict.sav'
restore, dir + 'ioverffac.sav'
restore, dir + 'phasecurves.sav'

if not keyword_exists(jj) then begin
  if not keyword_set(filtername) then jj = 0 else begin
    jj = where( filtertit eq filtername, count )
    if count ne 1 then begin
      print, 'No filter by input name '+filtername
      print, 'Possible filters include:'
      print, sindgen(1,n_elements(filtertit))+':  '+rotate(filtertit,1)
    endif
    jj = jj[0]
  endelse
endif

if strmid(filtertit[jj],0,1) eq 'E' then begin
  pp = pp_ee
  mumin = sin(!pi/180)
  instrument = 'WAC'
endif else begin
  pp = pp_gg
  mumin = sin(!pi/1500)
  instrument = 'NAC'
endelse
elevmin = asin(mumin) * 180 / !pi
if not keyword_set(gain) then gain = 12
if gain ne 12 and gain ne 29 and gain ne 95 and gain ne 215 and gain gt 0 then $
  stop, 'Gain should be 12, 29, 95, or 215.  ' +$
        'To skip this message, make gain a negative number.'
if not keyword_set(sum) then sum = 2
print, ''
if not keyword_set(elevation) then begin
  elevation = 1.
  print, 'No elevation angle specified.  Using elevation = '+$
         strtrim(elevation,2)+' degrees'
endif else begin
  if elevation ge elevmin then mumintxt = '' else begin
    mumintxt = ' (minimum '+strtrim(elevmin,2)+' degrees)'
  endelse
  print, 'Using input elevation = '+strtrim(elevation,2)+' degrees'+mumintxt
endelse
mu = sin(elevation*!pi/180) > mumin
if not keyword_set(phase) then phase = 90
norm = 3e-6

; Estimate bias and dark
bias = predict_bias( gain, sum, '', instrument, /silent )
bias = bias + 80 ; Very rough estimate of dark
instfrac = bias / 4096

pp = pp * norm / mu / (gain/12.) * (sum/2.)^2
ne1 = ioverffac[jj] / pp
ne2 = ioverffac[jj] / exp(interpol( alog(pp), alpha, phase ))

bgpadjust = n_elements(bgpredict) - n_elements(filtertit)
alpha_bg = (*(bgpredict[jj+bgpadjust]))[0,*]
ne_bg1 = (*(bgpredict[jj+bgpadjust]))[1,*]
ne_bg1 = ne_bg1 * (gain/12.) / (sum/2.)^2
ne_bg2 = exp(interpol( alog(ne_bg1), alpha_bg, phase ))
if keyword_set(nobg) then ne_bg2 = 1e20

if not keyword_set(frac) then begin
  if keyword_set(exposure) then begin
    frac = exposure * ( 1/ne2 + 1/ne_bg2 ) + instfrac
    print, 'Returning fractions using input exposure of '+$
           strtrim(exposure,2)+' sec'
  endif else begin
    frac = 0.5
    print, 'No saturation fraction specified.  Using frac = '+strtrim(frac,2)
  endelse
endif else print, 'Calculating exposures from saturation fraction '+$
                  strtrim(frac,2)
datafrac = frac - instfrac
exposure = datafrac / ( 1/ne2 + 1/ne_bg2 )
ringfrac = exposure / ne2
bgfrac = exposure / ne_bg2
if keyword_set(nobg) then begin
  bgfrac = 0.
  bgtag = ' (set to zero)'
endif else bgtag = ''

print, 'Gain = '+strtrim(gain,2)+'  /  Sum = '+strtrim(sum,2)+$
       '  /  Phase = '+strtrim(phase,2)+' degrees'
print, 'Filter:  '+filtertit[jj]+' (jj='+strtrim(jj,2)+')'
print, '----------'
print, 'Rings Saturation Fraction:  '+strtrim(ringfrac,2)+$
       ' ('+strtrim(round(ringfrac*4096),2)+' DN)'
print, 'Background Saturation Fraction:  '+strtrim(bgfrac,2)+bgtag
print, 'Total Data Saturation Fraction:  '+strtrim(datafrac,2)
print, 'Instrument Saturation Fraction:  '+strtrim(instfrac,2)
print, 'Total Saturation Fraction:  '+strtrim(frac,2)+$
       ' ('+strtrim(round(frac*4096),2)+' DN)'
print, 'Exposure:  '+strtrim(exposure,2)+' sec'
print, ''

if not keyword_exists(noplot) then noplot = 1
if not keyword_set(noplot) then begin
  nene = [ ne1, reform(ne_bg1) ]
  yr = reverse(10^( ( max(alog10([nene])) - min(alog10([nene])) )*[-.1,.1] + $
                    [ min(alog10([nene])), max(alog10([nene])) ] ))
  plot, [0], [0], /xs, /ys, xr=[0,180], yr=yr, /ylog, xticki=30, $
        xtit='Phase Angle', ytit='Saturation Exposure (sec)'
  oplot, alpha, ne1
  oplot, alpha_bg, ne_bg1, l=2
  oplot, [phase,phase,phase], [ne2,ne_bg2,exposure], ps=4
endif

if keyword_set(debug) then stop

end
