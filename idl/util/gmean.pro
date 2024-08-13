function gmean, xx

foo = where( xx gt 0, count )
if count ne n_elements(xx) then begin
  if count eq 0 then begin
    print, 'GMEAN: Input array has no positive elements; cannot compute geometric mean.'
    return, -1
  endif else begin
    print, 'GMEAN: Input array has some non-positive elements; using only positive elements to compute geometric mean.'
  endelse
endif

return, exp( mean( alog(xx[foo]) ) )

end
