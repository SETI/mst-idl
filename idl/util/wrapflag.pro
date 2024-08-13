function wrapflag, ang, radians=radians

if n_params() eq 0 then begin
  print, 'Syntax:  Result = WRAPFLAG( ang, /radians )'
  print, 'Returns an array of indices indicating points at which ang changes by'
  print, 'more than 180 degrees.  Set radians to use 2*pi instead.'
  return, -1
endif

np = n_elements(ang)
wrap = [ 0, where( abs(ang[1:np-1]-ang) gt 180, count ) + 1, np ]
if count eq 0 then wrap = [ 0, np ]
return, wrap

end
