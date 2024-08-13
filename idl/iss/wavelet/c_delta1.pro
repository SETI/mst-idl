nn = 300
tt = findgen(nn)/nn*12 - 6
dt = tt[1] - tt[0]
;; Attempt 1
;omega = findgen(nn/2+1) / nn / dt
;omega = [ omega, -reverse( (findgen((nn+1)/2-1)+1) / nn / dt ) ]
;; Attempt 2
;omega = 2 * !pi * findgen(nn) / nn / dt
;omega[nn/2:nn-1] = -omega[nn/2:nn-1]
;; Attempt 3
omega = (tt-min(tt)) * 2

redo:
if keyword_set(morlet) then omega0 = 6
if keyword_set(morlet) then psihat01 = exp(-(omega-omega0)^2/2)*!pi^(.25)
if keyword_set(morlet) then cdexp = 0.776
if keyword_set(morlet) then psi00 = !pi^(-.25)
if keyword_set(paul4) then mm = 4
if keyword_set(paul4) then psihat01 = (2.*omega)^mm / $
  sqrt(mm*factorial(2*mm-1)) * exp(-omega)
if keyword_set(paul4) then cdexp = 1.132
if keyword_set(paul4) then psi00 = 1.079
if keyword_set(mexicanhat) then omega = omega - max(omega)/2
if keyword_set(mexicanhat) then mm = 2
;if keyword_set(mexicanhat) then psihat01 = (-complex(0,1)*omega)^mm / $
;  sqrt(gamma(mm+.5)) * exp(-omega^2/2)
if keyword_set(mexicanhat) then psihat01 = (omega)^mm / sqrt(gamma(mm+.5)) * $
  exp(-omega^2/2)
if keyword_set(mexicanhat) then cdexp = 3.541
if keyword_set(mexicanhat) then psi00 = 0.867
; DOG6
if keyword_set(dog6) then omega = omega - max(omega)/2
if keyword_set(dog6) then mm = 6
;if keyword_set(dog6) then psihat01 = (-complex(0,1)*omega)^mm / $
;  sqrt(gamma(mm+.5)) * exp(-omega^2/2)
if keyword_set(dog6) then psihat01 = (omega)^mm / sqrt(gamma(mm+.5)) * $
  exp(-omega^2/2)
if keyword_set(dog6) then cdexp = 1.966
if keyword_set(dog6) then psi00 = 0.884
if not keyword_set(psihat01) then begin
  morlet = 1
  goto, redo
endif

;psihat01 = psihat01 * 2 * !pi
foo = where( omega gt 0 )
c_delta = int_tabulated( omega[foo], (psihat01/omega)[foo] ) 
help,c_delta,c_delta/cdexp,c_delta/cdexp/psi00
if not keyword_set(noplot) then plot, omega/2/!pi, psihat01, xr=[-2,2], $
  xtit='sw / 2 pi', ytit='psihat01'

end
