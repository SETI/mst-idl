function strsn, num, ndec=ndec, d2=d2, show1=show1

; Accepts input of a floating point number and converts it to a 
; more easily read string.  Keyword ndec specifies number of decimal places.

if n_params() eq 0 then begin
  print, 'Syntax:  string = strsn( num, [ ndec=ndec, d2=d2, show1=show1 ] )'
  print, '     ndec is # of decimal places, d2 uses positioning for exponent,'
  print, '     show1 forces argument to be shown, even if 1.0'
  retall
endif

if not keyword_exists(ndec) then ndec = 2

if (where(num ne 0))[0] eq -1 then return, replicate( '0', n_elements(num) )

if num lt 0 then begin
  neg = '"-",'
  num = -num
endif else neg = ''
exp = alog10(num)
foo = where( exp lt 0 and fix(exp) ne exp, count )
if count gt 0 then exp[foo] = exp[foo] - 1
exp = fix(exp)
arg = num / 10.^exp

if keyword_exists(ndec) then begin
  if keyword_set(ndec) then begin
    arg = string( arg, format='('+neg+'F'+strtrim(ndec+2,2)+'.'+$
                                   strtrim(ndec,2)+')' )
  endif else begin
    arg = strtrim( round(arg), 2 )
  endelse
endif else begin
  arg = strtrim( arg, 2 )
endelse

if keyword_set(d2) then begin
  txt1 = 'x'
  txt2 = '10!U'
  txt3 = '!N'
endif else begin
  txt1 = ' x '
  txt2 = '10^'
  txt3 = ''
endelse
out = arg + txt1 + txt2 + strtrim( exp, 2 ) + txt3
foo = where( arg eq 1., count )
if count gt 0 and not keyword_set(show1) then begin
  out[foo] = txt2 + strtrim( exp, 2 ) + txt3
endif

return, out

end
