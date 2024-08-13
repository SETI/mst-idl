pro quadratic_holdzero2, x, pp, y, pder

; This is a quadratic function, configured for use with MPCURVEFIT, constrained
; such that x = y = 0 when dy/dx = 0.  

y = pp*x^2

pder = x^2

end
