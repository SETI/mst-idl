; This is my attempt to duplicate the derivation of C_delta by
; Torrence and Compo 1998 (TC98).  

; Prepare plot
!p.thick = 2
!x.thick = 2
!y.thick = 2
!p.font = 1

; Number of elements in our time series
; Increasing cap_n extends the coverage on the left side of the curve
cap_n = 100000;300
; Time index array
nn = dindgen(cap_n)
; Scale index array
kk = dindgen(cap_n)
; Time array
tt = nn/cap_n
deltat = tt[1]
; 20 samples per octave, better than the 8 recommended in TC98 p.67
; Decreasing deltaj causes the sampling to be denser.  
deltaj = .05d
; TC98 p.67, "s0 should be chosen so that the equivalent Fourier period is
; approximately 2*deltat."  For omega0=6, lambda_fourier = 1.03 s
; Decreasing s0 extends the coverage on the left side of the curve.
; Let's decrease by factor of 10, to ensure we're getting all the curve.
s0 = 2*deltat / 10;2
; TC98 Eq.10
cap_j = (1/deltaj) * alog( cap_n * deltat / s0 ) / alog(2)
jj = dindgen(cap_j+1)
ss = s0 * 2^( jj*deltaj )
; TC98 Eq.5
omega = dindgen(cap_n/2+1) / cap_n / deltat
omega = [ omega, -reverse( (dindgen((cap_n+1)/2-1)+1) / cap_n / deltat ) ] 
omega = omega * 2 * !dpi

if keyword_set(iterate) then begin
  if not keyword_set(minomega0) then minomega0 = 5
  if not keyword_set(maxomega0) then maxomega0 = 20
  if not keyword_set(domega0) then domega0 = 0.2
  nomega0 = ( maxomega0 - minomega0 )/domega0 + 1
  _omega0 = findgen(nomega0)*domega0 + minomega0
  cd2 = dblarr(nomega0)
  oplot = 0
  j = -1
  nextomega0:
  j = j + 1
  omega0 = _omega0[j]
endif
; Calculate wavelet base functions as a cap_n-by-cap_j array
if not keyword_set(omega0) then omega0 = 6
w_delta = dblarr(cap_j+1)
; This applies Heaviside step function to psihat0
foo = where( omega gt 0 )
for x=0,cap_j do begin
  ; TC98 Table I
  psihat0 = !dpi^(-.25) * exp( -(ss[x]*omega[foo]-omega0)^2/2 )
  ; TC98 Eq.6
  psihat = sqrt(2*!dpi*ss[x]/deltat) * psihat0
  ; TC98 Eq.12
  w_delta[x] = total(psihat) / cap_n 
endfor
; TC98 Eq.13
c_delta = total( w_delta/sqrt(ss) * deltaj * sqrt(deltat) * !dpi^0.25 )
help, c_delta
; C_DELTA         DOUBLE    =       0.43918080

if keyword_set(noplot) then begin
endif else if keyword_set(oplot) then begin
  oplot, ss/deltat, w_delta/sqrt(ss) * deltaj * sqrt(deltat) * !dpi^0.25
endif else begin
  plot, ss/deltat, w_delta/sqrt(ss) * deltaj * sqrt(deltat) * !dpi^0.25, $
        /xlog, /ylog, xtit='s / !Mdt', $
        ytit='W!D!Md!N / sqrt( s / !Mdt ) * !Mp!U-1/4!N * !Mdj'
endelse

if keyword_set(iterate) then begin
  cd2[j] = c_delta
  oplot = 1
  if omega0 lt maxomega0 then goto, nextomega0
endif

end
