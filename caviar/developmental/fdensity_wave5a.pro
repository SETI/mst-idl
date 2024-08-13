pro fdensity_wave5a, rr, pp, vmodel, pder

; Drives fdensity_wave5 for use in mpcurvefit, solving for A and xi_d.

common fdensity_wave55, dw55_mm, dw55_phase0, dw55_rres, dw55_sigma

vmodel = fdensity_wave5( rr, a=pp[0], xi_d=pp[1], mm=dw55_mm, phi=dw55_phase0, $
                rres=dw55_rres, sigma=dw55_sigma, pder=pder )

end
