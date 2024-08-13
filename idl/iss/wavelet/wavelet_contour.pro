pro wavelet_contour, wave, rr, wavenum, level, nosqrt=nosqrt

if n_params() eq 0 then begin
  print, 'SYntax:  WAVELET_CONTOUR, wave, rr, wavenum, level'
  print, 'Overplot a line indicating the contour given by level'
  retall
endif

power = abs(wave)
if not keyword_exists(nosqrt) then nosqrt = 1
if keyword_set(nosqrt) then power = power^2
contour, alog10(power), rr, wavenum, /overplot, level=level

end
