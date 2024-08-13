function reconstruct_fft, fourier

if n_params() eq 0 then begin
  print, 'Syntax:  y = RECONSTRUCT_FFT( fourier )'
  retall
endif

y = fft( fourier, 1 )  ; Reverse Fourier Transform
foo = where( real_part(y) lt 0, count )
y = abs(y)
if count gt 0 then y[foo] = -y[foo]

return, y

end
