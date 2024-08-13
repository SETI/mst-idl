cap_n = 3000
tt = findgen(cap_n)/4
dt = tt[1] - tt[0]
;omega = 2 * !pi * dindgen(cap_n) / cap_n / dt
;omega[cap_n/2:cap_n-1] = -omega[cap_n/2:cap_n-1]
omega = dindgen(cap_n/2+1) / cap_n / dt
omega = [ omega, -reverse( (dindgen((cap_n+1)/2-1)+1) / cap_n / dt ) ] 
omega = omega * 2 * !dpi

redo:
if keyword_set(morlet) then begin
  omega0 = 6
  psihat01 = exp(-(omega-omega0)^2/2)*!pi^(.25)
  cdexp = 0.776
  psi00 = !pi^(-.25)
endif else if keyword_set(paul4) then begin
  mm = 4
  psihat01 = (2.*omega)^mm / $
             sqrt(mm*factorial(2*mm-1)) * exp(-omega)
  cdexp = 1.132
  psi00 = 1.079
endif else if keyword_set(mexicanhat) then begin
  omega = omega - max(omega)/2
  mm = 2
  psihat01 = (omega)^mm / sqrt(gamma(mm+.5)) * $
             exp(-omega^2/2)
  cdexp = 3.541
  psi00 = 0.867
endif else if keyword_set(dog6) then begin
  omega = omega - max(omega)/2
  mm = 6
  psihat01 = (omega)^mm / sqrt(gamma(mm+.5)) * $
             exp(-omega^2/2)
  cdexp = 1.966
  psi00 = 0.884
endif
if not keyword_set(psihat01) then begin
  morlet = 1
  goto, redo
endif

;psihat01 = psihat01 * 2 * !pi
foo = where( omega gt 0 )
c_delta = int_tabulated( omega[foo], (psihat01/omega)[foo] ) 
help,c_delta,c_delta/cdexp,c_delta/cdexp/psi00
if not keyword_set(noplot) then plot, omega/2/!pi, psihat01, $
  xr=[0,max(omega/2/!pi)], xtit='sw / 2 pi', ytit='psihat(0,1)'

end
