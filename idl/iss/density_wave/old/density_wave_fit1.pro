; Begin by typing:
; common fdensity_wave11, m, phi, rsat, thoukm, rres
; .run density_wave1
; .run density_wave_fit1
; To see the results:
; fdensity_wave1, r, aa1, out
; oplot, r, out, co=ctred()
; oplot, r, yfit, co=ctgreen()

weights = replicate( 1., n_elements(r) )
;aa = [ sigma, satmass, xi_d ]
;aa = [ sigma*.9, satmass*.9, xi_d*.9 ]
aa = [ 3e11, 7.182e19, 6.26 ]
aa1 = aa
yfit = curvefit( r, tau_norm, weights, aa, ssigma, function_name='fdensity_wave1' )

end
