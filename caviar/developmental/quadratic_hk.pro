pro quadratic_hk, x, pp, y, pder

; This is a pure quadratic function, configured for use with MPCURVEFIT.  
; In contrast to quadratic.pro, this one is in hk-format. 
; I have found that:
; g = MPCURVEFIT( r, v, weights, pp1, function_name='quadratic' )
; gives the same result as 
; pp2 = SVDFIT( r, v, 3 )
; with identical values for sigma and chisq (caveat below).
; Similarly, 
; g = MPCURVEFIT( r, v, weights, pp3, function_name='quadratic_hk' )
; gives a result that follows these relations:
; pp3[0] = pp1[0] - pp1[1]^2/4/pp1[2]
; pp3[1] = -pp1[1]/2/pp1[2]
; pp3[2] = pp1[2]
; NOTE: SVDFIT automatically multiplies sigma by sqrt(chisq/DOF), while you 
;       must do this manually w ith MPCURVEFIT.  
; Also, remember to define beforehand weights = replicate(1.,n_elements(r))

y = pp[0] + pp[2]*( x - pp[1] )^2

pder = [ [replicate(1.0d0,n_elements(x))], [-2*pp[2]*(x-pp[1])], [(x-pp[1])^2] ]

end
