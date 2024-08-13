if keyword_set(atlas54) then begin

  m = 5
  rres = (ring_rads[where(ring_rads_legend eq 'At 5:4')])[0]
  sigma = 1.25  ;Initial guess
  slope = 2*!pi/3.08/rres^4*60330.^4*(m-1)/sigma
  r1 = min( where(r ge rres-thoukm) )
  k = poly( r[r1:nt-1]+thoukm, [-slope*rres,slope] )
  r2 = r1 + max( where(k le yr[1]) )
  ;oplot, r[r1:r2], k
  ;!p.multi=0
  ;for j=r1,r2 do begin
  ;  plot, period, abs(wave[j,*]^2), yr=[0,1e-5]
  ;  oplot, k[j-r1]*[1,1], [0,1e-5], l=1
  ;  wait, .1
  ;endfor

  ; The peak we want is always the highest point with a wavenumber above 1.
  ; period[0:47] are greater than one, period[48:100] are less.
  r1 = 110
  r2 = 132
  int = indgen(48)
  _peak = fltarr(nt)
  for j=r1,r2 do begin
    amp = abs(wave[j,*]^2)
    damp = deriv( amp, period )
    peak = (where( amp[int] eq max(amp[int]) ))[0]
    _peak[j] = peak
    ;rr1 = max(where( int lt peak and damp[int] gt 0 ))
    ;if rr1 eq -1 then rr1 = 0
    ;rr2 = min( where(int gt peak and damp[int] lt 0 ))
    ;if rr2 eq -1 then rr2 = max(int)
    ;plot, period[int[rr1:rr2]], amp[int[rr1:rr2]],xr=[0,6]
    ;oplot, period[peak[[0,0]]], [0,1], l=1
    ;wait,.1
  endfor
  ;oplot, r[r1:r2], period[_peak[r1:r2]]
  kfit = poly_fit( r[r1:r2], period[_peak[r1:r2]], 1 )
  ;oplot, r[r1:r2], poly( r[r1:r2], kfit )
  print,'Calculated sigma = '+strtrim(2*!pi/3.08/rres^4*60330.^4*(m-1)/kfit[1],2)
  if keyword_set(kfit1) then oplot, r[r1:r2], poly( r[r1:r2], kfit1 )
  if !d.name eq 'PS' then stop

endif

end
