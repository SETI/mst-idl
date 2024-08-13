function fft_filter, _fourier, ff, minlam=minlam, maxlam=maxlam

if n_params() eq 0 then begin
  print, 'Syntax:  Result = FFT_FILTER( fourier, ff, minlam=, maxlam= )'
  return, -1
endif

fourier = _fourier
lambda = 1./abs(ff)
if keyword_set(minlam) then begin
  foo = where( lambda lt minlam, count )
  if count gt 0 then lambda[foo] = 0
endif
if keyword_set(maxlam) then begin
  foo = where( lambda gt maxlam, count )
  if count gt 0 then lambda[foo] = 0
endif
fourier = fourier[ where(lambda ne 0) ]
lambda = lambda[ where(lambda ne 0) ]
yfilt = reconstruct_fft( fourier )

return, yfilt

end
