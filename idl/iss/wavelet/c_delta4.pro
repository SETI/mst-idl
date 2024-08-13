; This is the calculation of C_delta using the methods outlined in my paper

; Find a time array that results in good sampling of the frequency region
; in which the Fourier transform of the mother wavelet has power.  
cap_n = 30000
tt = findgen(cap_n)/10
dt = tt[1] - tt[0]
; Related Fourier wavelengths, as in TC98 Eq.5
;omega = 2 * !pi * dindgen(cap_n) / cap_n / dt
;omega[cap_n/2:cap_n-1] = -omega[cap_n/2:cap_n-1]
omega = dindgen(cap_n/2+1) / cap_n / dt
omega = [ omega, -reverse( (dindgen((cap_n+1)/2-1)+1) / cap_n / dt ) ] 
omega = omega * 2 * !dpi
if keyword_set(iterate) then begin
  if not keyword_set(minomega0) then minomega0 = 5
  if not keyword_set(maxomega0) then maxomega0 = 20
  if not keyword_set(domega0) then domega0 = 0.2
  nomega0 = ( maxomega0 - minomega0 )/domega0 + 1
  _omega0 = findgen(nomega0)*domega0 + minomega0
  cd4 = dblarr(nomega0)
  oplot = 0
  j = -1
  nextomega0:
  j = j + 1
  omega0 = _omega0[j]
endif
; Parameters for Morlet
if not keyword_set(omega0) then omega0 = 6
psi00 = !pi^(-.25)
; My Eq.14, with r=0 and s=1
psihat01 = sqrt(2) * !pi^(.25) * exp(-(omega-omega0)^2/2)
; My Eq.13
foo = where( omega gt 0 )
c_delta = int_tabulated( omega[foo], (psihat01/omega)[foo] ) 
help,c_delta
; C_DELTA         FLOAT     =      0.810567

if not keyword_set(ylog) then ylog = 0
if keyword_set(noplot) then begin
endif else if keyword_set(oplot) then begin
  oplot, omega/2/!pi, psihat01/omega
endif else begin
  plot, omega/2/!pi, psihat01/omega, ylog=ylog, $
        xr=[0,max(omega/2/!pi)], xtit='s!Mw / 2 pi', ytit='psihat(0,1) / !Mw'
endelse

if keyword_set(iterate) then begin
  cd4[j] = c_delta
  oplot = 1
  if omega0 lt maxomega0 then goto, nextomega0
endif

end
