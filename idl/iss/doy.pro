function doy, _num, leap=leap, month=month, day=day, getdoy=getdoy, $
              short=short

if n_params() eq 0 and not keyword_set(getdoy) then begin
  print, 'Syntax:  Result = doy( num, [/leap] )'
  retall
endif

if keyword_set(short) then begin
  monthnames = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', $
	'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec' ]
endif else begin
  monthnames = [ 'January', 'February', 'March', 'April', 'May', 'June', $
	'July', 'August', 'September', 'October', 'November', 'December' ]
endelse
days1 = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
days2 = [ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

if keyword_set(getdoy) then begin
  if keyword_set(leap) then begin
    if n_elements(leap) gt 1 then stop, 'Not implemented for this option.'
    days = days2
  endif else days = days1
  if month eq 1 then doy = 0 else doy = total(days[0:month-2])
  doy = doy + day
  return, doy
endif

num = fix(_num)
sz = size(num)
nn = n_elements(num)
out = strarr(nn)
month = intarr(nn)
day = intarr(nn)
for k=0,nn-1 do begin
  j=0
  if keyword_set(leap) then begin
    if n_elements(leap) eq nn then begin
      if leap[k] eq 1 then days = days2 else days = days1
    endif else days = days2
  endif else days = days1
  while num[k] gt days[j] do begin
    num[k] = num[k] - days[j]
    j = j + 1
  endwhile
  out[k] = monthnames[j] + ' ' + strtrim(num[k],2)
  month[k] = j+1
  day[k] = num[k]
endfor
if sz[0] eq 0 then out = out[0]  ;scalar

return, out

end
