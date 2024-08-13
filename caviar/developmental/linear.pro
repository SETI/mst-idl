pro linear, x, pp, y, pder

; This is a pure linear function, configured for use with MPCURVEFIT.  
; I have found that:
; fit = MPCURVEFIT( r, v, weights, pp, function_name='linear' )
; gives the same result as 
; pp = SVDFIT( r, v, 2, measure_errors=errbars )
; with identical values for sigma and chisq (caveat below).
; NOTE: SVDFIT automatically multiplies sigma by sqrt(chisq/DOF), while you 
;       must do this manually w ith MPCURVEFIT.  
; Also, remember to define beforehand weights = replicate(1.,n_elements(r))
; or, if you have error bars and used measure_errors in SVDFIT, 
; weights = 1.0d0/errbars^2

y = pp[0] + pp[1]*x

pder = [ [replicate(1.0d0,n_elements(x))], [x] ]

end
