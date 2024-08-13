pro fdensity_wave5b, rr, pp, vmodel, pder

; Drives fdensity_wave5 for use in mpcurvefit, solving for A only.

common fdensity_wave55, dw55_mm, dw55_phase0, dw55_rres, dw55_sigma, dw55_xi_d

vmodel = fdensity_wave5( rr, a=pp[0], xi_d=dw55_xi_d, mm=dw55_mm, $
                phi=dw55_phase0, $
                rres=dw55_rres, sigma=dw55_sigma, pder=pder )

; fdensity_wave5 calculates derivatives wrt both A and xi_d
pder = pder[*,0]

end
