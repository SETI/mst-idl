function wavelet_ridges, wave, rr, wavenum, minpower=minpower, $
  highest=highest, doplot=doplot, gauss=gauss, ridgepower=ridgepower

if n_params() eq 0 then begin
  print, 'Syntax:  Result = WAVELET_RIDGES( wave, rr, wavenum, minpower=, /highest )'
  print, 'For input n-by-m array wave, returns a n-by-2 array with the first two rows being the x- and y-index of a local maximum in the wavelet power.  For optionanl input n-element vector rr, and m-element vector wavenum, rows 3 and 4 contain the interpolated coordinates.  Switch highest to include only the highest value of the power for each x-index.  Input minpower discards all local maxima with power values lower than minpower.'
  retall
endif

power = abs(wave^2)
sz = size(power)
; According to Equation 11 of Addison et al 2002, power should be divided by
; the scale a.  But scale is directly proportional to the Fourier period, 
; which in turn is inversely proportional to the wavenumber.  Since we're only
; looking for zero-derivative points, constant factors are irrelevant, and we
; can simply multiply by the wavenumber.
scpower = power * rebin( rebin(wavenum,1,sz[2]), sz[1], sz[2] )

xx = 2
yind = lindgen(sz[2])
out = [ [0], [0] ]
if keyword_set(gauss) then ridgepower = 0.
for j=0,sz[1]-1 do begin
  dd = deriv(scpower[j,*])
  d2 = deriv(dd)
  ; Find places where derivative crosses from +ve to -ve
  mkexed, where( dd gt 0 ), exed, z
  ; Crossing is always left of the point in exed
  exed[1,*] = exed[1,*] + 1
  exed = reform(exed,2*(z+1))
  ; Only points more than xx away from the edges
  ; (If all points fail, then go to the next j)
  foo = where( exed gt xx and exed lt sz[2]-xx, count )
  if count gt 0 then begin
    zero = float(exed[foo])
    ; Check that power is greater than minpower
    if not keyword_set(minpower) then minpower = min(power)
    foo = where( power[j,zero] ge minpower, count )
    if count gt 0 then begin
      zero = zero[foo]
      ; Check that more than xx away from another zero crossing
      bad = zero*0
      for k=0,n_elements(zero)-1 do begin
        foo = where( abs(exed-zero[k]) le xx, count )
        if count gt 1 then bad[k] = 1
      endfor
      foo = where( bad eq 0, count )
      if count gt 0 then begin
        zero = zero[foo]
        ; Check that second derivative is negative
        foo = where( d2[zero] lt 0, count )
        if count gt 0 then begin
          zero = zero[foo]
          if n_elements(zero) gt 1 then if keyword_set(highest) then begin
            zero = zero[where( scpower[j,zero] eq max(scpower[j,zero]), count )]
          endif
          ; Interpolate to get precise location of zero crossing
          for k=0,count-1 do begin
            zero[k] = interpol( yind[zero[k]-xx:zero[k]+xx], $
                                deriv(scpower[j,zero[k]-xx:zero[k]+xx]), 0 )
            out = [ out, [[j],[zero[k]]] ]
            if keyword_set(gauss) then begin
              ; The ridgepower obtained by this method is somewhat smoother
              ; than the method in wavelet_ridgepower.pro, but not enough
              ; to prevent the need for smoothing.
              ridgepower = [ ridgepower, 0 ]
              foo = where( scpower[j,*] ge max(scpower[j,*])/2, count )
              if count gt 3 then begin
                nrp = n_elements(ridgepower)
                ; Use nterms=3 rather than 6, so that fewer j's are disqualified
                ;yfit = gaussfit( yind[foo], scpower[j,foo], a, nterms=6 )
                ;ridgepower[nrp-1] = a[0]*exp(-((zero[k]-a[1])/a[2])^2/2)+$
                ;                    a[3]+a[4]*zero[k]+a[5]*zero[k]^2
                yfit = gaussfit( yind[foo], scpower[j,foo], a, nterms=3 )
                ridgepower[nrp-1] = a[0]
                ; This gives a ridge trace that is not better than the 
                ; previous method, and many data points are disqualified.
                ;if abs(a[1]-zero[k]) lt 2 then out = [ out, [[j],[a[1]]] ]
              endif
            endif
            if keyword_set(doplot) then begin
              plot, yind, scpower[j,*], /xs, /ys, yr=[0,max(scpower)]
              if keyword_set(gauss) then begin
                oplot, yind[foo], yfit, co=red()
                oplot, [zero[k]], [ridgepower[nrp-1]], ps=4, co=red()
              endif else oplot, [zero[k]], [max(scpower[j,*])], ps=4, co=red()
              wait, doplot
            endif
          endfor
        endif
      endif
    endif
  endif
endfor

out = out[1:n_elements(out[*,0])-1,*]
if keyword_set(gauss) then ridgepower = clip(ridgepower)
if keyword_set(rr) then begin
  if n_elements(rr) ne sz[1] then stop, 'WAVELET_RIDGES: Size of rr'
  out = [ [out], [interpol( rr, lindgen(sz[1]), out[*,0] )] ]
endif else out = [ [out], [replicate(0,sz[1])] ]
if n_elements(wavenum) ne sz[2] then stop, 'WAVELET_RIDGES: Size of wavenum'
out = [ [out], [interpol( wavenum, lindgen(sz[2]), out[*,1] )] ]

return, out

end
