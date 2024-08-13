function path_length, xy

if n_params() eq 0 then begin
  print, 'Syntax:  Result = PATH_LENGTH( xy )'
  print, 'Input xy is an n-by-2 vector.  Top row is x, bottom row is y.'
  retall
endif

n = n_elements(xy[*,0])
out = total(sqrt( (xy[1:n-1,0]-xy[0:n-2,0])^2 + (xy[1:n-1,1]-xy[0:n-2,1])^2 ))

return, out

end
