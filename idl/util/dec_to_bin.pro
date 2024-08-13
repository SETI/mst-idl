function dec_to_bin, n, returnstring=returnstring

if n_params() eq 0 then begin
  print, 'Syntax:  n2 = dec_to_bin( n10, [/returnstring] )'
  retall
endif

sz = size(n)
if sz[0] ne 0 then begin
  print, 'n must be a scalar'
  return, n
endif

placevalues = 2. ^ indgen(31)

if n eq 0 then return, 0

if n gt 65536 then begin
  ;; The highest you can go with a numerical output is 65536.  Beyond that, 
  ;; the output must be a string.
  ;print, 'n = '+strtrim(n,2)+' is too high, cannot convert to binary'
  ;return, n
  returnstring = 1
endif

a = double((where(placevalues gt n))[0] - 1)
while min(a) ne 0 and min(a) ne -1 do begin
  m = n mod placevalues(min(a))
  a = [ a, (where(placevalues gt m))[0] - 1 ]
endwhile
if min(a) eq -1 then a=a(0:n_elements(a)-2)
if keyword_set(returnstring) then begin
  bit = [ '0', '1' ]
  b = '1'
  for j=max(a)-1,0,-1 do begin
    foo = where( a eq j, count )
    b = b + bit[ count ]
  endfor
  return, b
endif else begin
  return, total(10l^a)
endelse

end
