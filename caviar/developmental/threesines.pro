pro threesines, x, pp, y, pder

; This is a sum of three pure sine functions, configured for use with 
; MPCURVEFIT.  
; NOTE: SVDFIT automatically multiplies sigma by sqrt(chisq/DOF), while you 
;       must do this manually with MPCURVEFIT.  
; Also, remember to define beforehand weights = replicate(1.,n_elements(r))

y = pp[0] * sin( pp[1]*x + pp[2] ) + pp[3] * sin( pp[4]*x + pp[5] ) + $
    pp[6] * sin( pp[7]*x + pp[8] )
pder = [ [ sin( pp[1]*x + pp[2] ) ], $
         [ pp[0] * x * cos( pp[1]*x + pp[2] ) ], $
         [ pp[0] * cos( pp[1]*x + pp[2] ) ], $
         [ sin( pp[4]*x + pp[5] ) ], $
         [ pp[3] * x * cos( pp[4]*x + pp[5] ) ], $
         [ pp[3] * cos( pp[4]*x + pp[5] ) ], $
         [ sin( pp[7]*x + pp[8] ) ], $
         [ pp[6] * x * cos( pp[7]*x + pp[8] ) ], $
         [ pp[6] * cos( pp[7]*x + pp[8] ) ] ]

end
