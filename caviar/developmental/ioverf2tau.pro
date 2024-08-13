pro ioverf2tau, ioverf_d, tau1, tau2

; Takes I/F data and interpolates it into tau (pre-calculated by Luke Dones).
; For dark side of the rings, two possible solutions on either side of the
; contrast reversal.

big_tau = 10. ; arbitrary large number bigger than any optical depth in ring

; Read multiple-scattering data
; tau_1, tau_2, ioverf_1, ioverf_2
restore, '/home/borogove/iss/ioverf2tau/mult_scat.sav'

; Quick fix
foo = where( ioverf_d lt 0, count )
if count gt 0 then ioverf_d[foo] = 0
tau1 = interpol( tau_1, ioverf_1, ioverf_d )
tau2 = interpol( tau_2, ioverf_2, -ioverf_d )

end

