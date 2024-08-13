function bin_to_dec, n

; If digits other than 1 and 0 are used, they will be interpreted as 1's.

if n_params() eq 0 then begin
  print, 'Syntax:  n10 = dec_to_bin( n2 )'
  retall
endif

sz = size(n)
if sz[0] ne 0 then begin
  print, 'n must be a scalar'
  return, n
endif

if sz[1] eq 7 then begin

  a = double(strlen(n)) - 1
  for j=a-1,0,-1 do begin
    if strmid(n,a[0]-j,1) ne '0' then a = [ a, j ]
  endfor
  return, total(2^a)

endif else begin

  placevalues = dindgen(25)

  if n eq 0 then return, 0

  if n gt max(10^placevalues) then begin
    print, 'n = '+strtrim(n,2)+' is too high, cannot convert from binary'
    return, n
  endif else begin
    a = double((where(10^placevalues gt n))[0] - 1)
    while min(a) ne 0 and min(a) ne -1 do begin
      m = n mod 10^placevalues(min(a))
      a = [ a, (where(10^placevalues gt m))[0] - 1 ]
    endwhile
    if min(a) eq -1 then a=a(0:n_elements(a)-2)
    return, total(2^a)
  endelse

endelse

end
