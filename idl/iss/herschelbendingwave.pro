; Fits from Dick French's TS-V1864
; Apsidal rate for IER 18 Herschel Ringlet
pomegadot_m1 = 4.9674d0
; Node rate for IER 18 Herschel Ringlet
big_omegadot_m1 = -4.9261d0
; Semimajor axis for IER 18 Herschel Ringlet
a_m1 = 118234.37d0
omega_m1 = sqrt(caviar_omega2(a_m1)) * 180/!dpi*86400
; Apsidal rate for OER 17 Herschel Ringlet
pomegadot_m2 = 4.9529d0
; Node rate for OER 17 Herschel Ringlet
big_omegadot_m2 = -4.9226d0
; Semimajor axis for OER 17 Herschel Ringlet
a_m2 = 118263.22d0
omega_m2 = sqrt(caviar_omega2(a_m2)) * 180/!dpi*86400

;mu_m1 = omega_m1 + pomegadot_m1
;mu_m2 = omega_m2 + pomegadot_m2
;kappa_m1 = omega_m1 + big_omegadot_m1
;kappa_m2 = omega_m2 + big_omegadot_m2
;mu_m = mean([mu_m1,mu_m2])
;kappa_m = mean([kappa_m1,kappa_m2])
pomegadot_m = mean([pomegadot_m1,pomegadot_m2])
big_omegadot_m = mean([big_omegadot_m1,big_omegadot_m2])
omega_m = mean([omega_m1,omega_m2])

;patternspeed = -omega_m + big_omegadot_m + pomegadot_m
patternspeed = big_omegadot_m

rr = dindgen(250) + 118100.0d0
nu_r = sqrt(caviar_nu2(rr)) * 180/!dpi*86400
omega_r = sqrt(caviar_omega2(rr)) * 180/!dpi*86400
big_omegadot_r = omega_r - nu_r
plot_nosci, rr, big_omegadot_r, /xs, /ys, xtit='Radius (km)', $
            ytit='Nodal Precession Rate (!Uo!N/day)'
ieg = 118194.0d0
oeg = 118283.4d0
foo = where( rr lt ieg )
oplot, rr[foo], big_omegadot_r[foo], thick=10
foo = where( rr gt a_m1 and rr lt a_m2 )
oplot, rr[foo], big_omegadot_r[foo], thick=10
foo = where( rr gt oeg )
oplot, rr[foo], big_omegadot_r[foo], thick=10
oplot, [!x.crange[0],a_m1,a_m1], $
       [(sqrt(caviar_omega2(a_m1))-sqrt(caviar_nu2(a_m1)))*$
        180/!dpi*86400*[1,1],!y.crange[0]], l=1
oplot, [!x.crange[0],a_m2,a_m2], $
       [(sqrt(caviar_omega2(a_m2))-sqrt(caviar_nu2(a_m2)))*$
        180/!dpi*86400*[1,1],!y.crange[0]], l=1
oplot, [!x.crange[0],ieg,ieg], $
       [(sqrt(caviar_omega2(ieg))-sqrt(caviar_nu2(ieg)))*$
        180/!dpi*86400*[1,1],!y.crange[0]], l=1
oplot, [!x.crange[0],oeg,oeg], $
       [(sqrt(caviar_omega2(oeg))-sqrt(caviar_nu2(oeg)))*$
        180/!dpi*86400*[1,1],!y.crange[0]], l=1
oplot, !x.crange, patternspeed*[1,1], l=5
xyouts, a_m1, !y.crange[0] + (!y.crange[1]-!y.crange[0])*.1, orient=90, $
        'IER 18 Herschel Ringlet'
xyouts, a_m2, !y.crange[0] + (!y.crange[1]-!y.crange[0])*.1, orient=90, $
        'OER 17 Herschel Ringlet'
xyouts, oeg, !y.crange[0] + (!y.crange[1]-!y.crange[0])*.1, orient=90, $
        'OEG 16 Herschel Gap'
xyouts, ieg, !y.crange[0] + (!y.crange[1]-!y.crange[0])*.1, orient=90, $
        'IEG Herschel Gap'

end
