function prime_factors, _num

num = _num
if not wholenum(num) then stop, 'Input must be a whole number.'
num = double(num)
j = 2l
pf = 0l

while j le num/2 do begin

  if wholenum(num/j) then begin
    pf = [ pf, j ]
    num = num / j
  endif else begin
    j = j + 1
  endelse

endwhile

pf = [ pf, num ]
pf = long(clip(pf))

return, pf

end
