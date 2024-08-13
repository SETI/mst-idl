pro sine5, x, pp, y, pder

; This is a pure sine function, plus a linear function, 
; configured for use with MPCURVEFIT.  
; NOTE: SVDFIT automatically multiplies sigma by sqrt(chisq/DOF), while you 
;       must do this manually w ith MPCURVEFIT.  
; Also, remember to define beforehand weights = replicate(1.,n_elements(r))

y = pp[0] * sin( pp[1]*x + pp[2] ) + pp[3] + pp[4]*x
pder = [ [ sin( pp[1]*x + pp[2] ) ], $
         [ pp[0] * x * cos( pp[1]*x + pp[2] ) ], $
         [ pp[0] * cos( pp[1]*x + pp[2] ) ], $
         [ replicate( 1, n_elements(x) ) ], $
         [ x ] ]

end
