function make_pointer_array, n, value=value

if n_params() eq 0 then begin
  print, 'Syntax:  Result = MAKE_POINTER_ARRAY( n, value= )'
  print, 'Creates an array of pointers, ready to be filled with variables.'
  retall
endif

out = ptrarr( n, /allocate_heap )
if keyword_set(value) then for j=0,n-1 do *(a[j]) = value

return, out

end
