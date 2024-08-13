pro quadratic_holdzero3, x, pp, y, pder

; This is a quadratic function, configured for use with MPCURVEFIT, constrained
; such that x = 0 when dy/dx = 0.  

y = pp[0] + pp[1]*x^2

pder = [ [replicate(1.0d0,n_elements(x))], [x^2] ]

end
