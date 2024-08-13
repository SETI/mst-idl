function sinc, _xx

xx = float(_xx)
out = sin(!pi*xx) / !pi / xx
foo = where( xx eq 0, count )
if count gt 0 then out[foo] = 1

return, out

end
