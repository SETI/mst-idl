pro quadratic_holdzero, x, pp, y, pder

; This is a quadratic function, configured for use with MPCURVEFIT, constrained
; such that y = 0 when dy/dx = 0.  

y = pp[0]^2/4/pp[1] + pp[0]*x + pp[1]*x^2

pder = [ [ x + 2*pp[0]/4/pp[1] ], [ x^2 - pp[0]^2/4/pp[1]^2 ] ]

end
