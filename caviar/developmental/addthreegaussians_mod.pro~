pro addthreegaussians, x, pp, y, pder

; This is a function adding three gaussians, configured for use with MPCURVEFIT.
; NOTE: SVDFIT automatically multiplies sigma by sqrt(chisq/DOF), while you 
;       must do this manually w ith MPCURVEFIT.  
; Also, remember to define beforehand weights = replicate(1.,n_elements(r))

z1 = ( x - pp[1] )/pp[2]
z2 = ( x - pp[4] )/pp[5]
z3 = ( x - pp[7] )/pp[8]
y = pp[0]*exp(-z1^2/2) + pp[3]*exp(-z2^2/2) + pp[6]*exp(-z3^2/2)

pder = [ [exp(-z1^2/2)], $
         [pp[0]*z1*exp(-z1^2/2)/pp[2]], $
         [pp[0]*z1*exp(-z1^2/2)*(x-pp[1])/pp[2]^2], $
         [exp(-z2^2/2)], $
         [pp[3]*z2*exp(-z2^2/2)/pp[5]], $
         [pp[3]*z2*exp(-z2^2/2)*(x-pp[4])/pp[5]^2], $
         [exp(-z3^2/2)], $
         [pp[6]*z3*exp(-z3^2/2)/pp[8]], $
         [pp[6]*z3*exp(-z3^2/2)*(x-pp[7])/pp[8]^2] ]

end
