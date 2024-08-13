pro run_fft, y, power, f, x=x, dx=dx, noplot=noplot, log=log, phase=phase, $
             fourier=fourier, ff=ff, gaussian=gaussian, coeff=coeff, $
             nonorm=nonorm

if n_params() eq 0 then begin
  print, 'Syntax:  RUN_FFT, y, power, f, x=x, dx=dx'
  retall
endif

n = n_elements(y)
if keyword_set(x) then begin
  if (where(x ne x[sort(x)]))[0] ne -1 then begin
    print, 'RUN_FFT: X was not in ascending order.  Now sorting both X and Y.'
    y = y[sort(x)]
    x = x[sort(x)]
  endif
  dx = mean( x[1:n-1] - x[0:n-2] )
endif else if not keyword_set(dx) then begin
  print, 'Neither X nor DX is set.  Assuming DX = 1.'
  dx = 1
endif

if dx le 0 then print, 'RUN_FFT: Warning: DX is not greater than zero.'

; Since n is a long, n/2 becomes n/2-.5 if n is odd.
f = findgen(n/2+1) / n / dx
f = [ f, -reverse( (findgen((n+1)/2-1)+1) / n / dx ) ]

fourier = fft( y )
power = abs(fourier^2)
power[1:n/2] = power[1:n/2] + reverse(power[n/2+.5:n-1])
power = power[1:n/2]
phase = get_angle( imaginary(fourier), real_part(fourier), /rad, /norm )
phase = phase[1:n/2]
ff = f
f = f[1:n/2]

if not keyword_set(noplot) then plot_fft, y, power, f, $
                                          coeff=coeff, nonorm=nonorm
if keyword_set(gaussian) then begin
  a = dblarr(6)
  gi = gaussfit( 1./f, power, a )
  if not keyword_set(noplot) then begin
    plot, 1./f, gi, /xlog, /xs, /ys, /nodata, /noerase
    oplot, 1./f, gi, co=green()
  endif
  print, 'Gaussian fit parameters:'
  print, a
  gaussian = a
endif

end
