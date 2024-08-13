; This is for 007/HIPHASE only.  For the 2009 equinox impacts, use
; fit_propellers_redge_peak3.pro

radscans = [ 'N1493791286_1_cal.scan3', 'N1493791286_1_cal.scan4', 'N1493791602_1_cal.scan1', 'N1493791602_1_cal.scan2', 'N1493792122_1_cal.scan1' ]
nradscans = n_elements(radscans)
_peak_ioverf = fltarr(nradscans)
_peak_tau = fltarr(nradscans)
_ea = fltarr(nradscans)
for j=0,nradscans-1 do begin
  radscan_xr = 0
  radscan_yr = 0
  restore, radscans[j]
  redo:
  @plot_radscan1
  print, 'Click on baseline'
  cursor, x, y, 3, /data
  foo = abs( tkm(radi) - x )
  x1 = (where( foo eq min(foo) ))[0]
  plots, [tkm(radi[x1])], [val[x1]], ps=4, color=red()
  cursor, x, y, 3, /data
  foo = abs( tkm(radi) - x )
  x2 = (where( foo eq min(foo) ))[0]
  plots, tkm(radi[[x1,x2]]), val[[x1,x2]], ps=-4, color=red()
  reply = ''
  while reply eq '' do begin
    print, 'Continue or redo? (c/r)'
    read, reply
    if reply eq 'r' then goto, redo else if reply eq 'q' then retall else if reply ne 'c' then reply=''
  endwhile
  good = lindgen(x2-x1+1) + x1
  baseline = findgen(x2-x1+1)/(x2-x1) * (val[x2]-val[x1]) + val[x1]
  _peak_ioverf[j] = max(val[good]-baseline)
  mu = 1
  pomega0 = 0.5
  phaseang = ([174.67,174.67,173.97,173.97,172.76])[j]
  phasefunc = 40.0
  const = 4*mu/pomega0/phasefunc
  _peak_tau[j] = _peak_ioverf[j] * const
  ew = int_tabulated( radi[good], val[good]-baseline ) * const
  mnlon = strmid( radscan_descrip, strpos(radscan_descrip,'mnlon')+8, 7 )
  mxlon = strmid( radscan_descrip, strpos(radscan_descrip,'mxlon')+8, 7 )
  _ea[j] = ew * (float(mxlon)-float(mnlon)) * !pi/180 * mean(radi[good])
  print, 'Peak I/F:  '+strtrim(_peak_ioverf[j],2)
  print, 'Peak tau:  '+strtrim(_peak_tau[j],2)
  print, 'Equivalent Area (tau*km^2):  '+strtrim(_ea[j],2)
endfor

end

