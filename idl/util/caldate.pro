function caldate, jdate, doy=_doy, reverse=reverse, nosec=nosec, nofracsec=nofracsec

if n_params() eq 0 then begin
  print, 'Syntax:  Result = CALDATE( jdate )'
  print, 'Converts Julian date into calendar date, returns a string in the format'
  print, 'yyyy MON dd hh:mm:ss.sss.  Alternatively, set /doy for format yyyy-doyThh:mm:ss.sss'
  return, -1
endif

if type(jdate) eq 4 then begin
  print, 'CALDATE:  Warning:  You should use double-precision floating point.'
endif

months = [ 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', $
           'OCT', 'NOV', 'DEC' ]
if keyword_set(reverse) then begin
  strdate = jdate
  year = strmid( strdate, 0, 4 )
  if keyword_set(_doy) then begin
    _doy = strmid( strdate, 5, 3 )
    leap = 1-sign( year mod 4 )
    foo = doy( _doy, month=mon, day=day, leap=leap )
    offs = 0
  endif else begin
    mon = strmid( strdate, 5, 3 )
    for j=0,n_elements(mon)-1 do mon[j] = where( mon[j] eq months )+1    
    day = strmid( strdate, 9, 2 )
    offs = 3
  endelse 
  hour = strmid( strdate, 9+offs, 2 )
  min = strmid( strdate, 12+offs, 2 )
  sec = strdate
  sec[*] = '0'
  foo = where( strlen(strdate) gt 13, count )
  if count gt 0 then sec[foo] = strmid( strdate[foo], 15+offs, 2 )
  out = julday( mon, day, year, hour, min, sec )
  return, out
endif

if keyword_set(nosec) then begin
  fosec = ')'
endif else if keyword_set(nofracsec) then begin
  fosec = ',":",I02)'
endif else begin
  fosec = ',":",F06.3)'
endelse

caldat, jdate, mon, day, year, hour, min, sec
if keyword_set(_doy) then begin
  leap = 1-sign( year mod 4 )
  _doy = doy( leap=leap, month=mon, day=day, /getdoy )
  fo = '(I4,"-",I03,"T",I02,":",I02'+fosec
  if keyword_set(nosec) then begin
    return, string( year, _doy, hour, min, fo=fo )
  endif else begin
    return, string( year, _doy, hour, min, sec, fo=fo )
  endelse
endif

out = strarr(1,n_elements(jdate))
for j=0,n_elements(jdate)-1 do begin
  if year[j] le 0 then begin
    fo = '(I4,"BC '
    year[j] = -year[j]
  endif else if year[j] ge 10000 then begin
    fo = '(I'+string(fix(alog10(year[j]))+1,fo='(I1)')+'," '
  endif else begin
    fo = '(I4," '
  endelse
  fo = fo + months[mon[j]-1]+' ",I02," ",I02,":",I02'+fosec
  if keyword_set(nosec) then begin
    out[j] = string( year[j], day[j], hour[j], min[j], fo=fo )
  endif else begin
    out[j] = string( year[j], day[j], hour[j], min[j], sec[j], fo=fo )
  endelse
endfor

if n_elements(out) eq 1 then out = out[0]
return, out

end
