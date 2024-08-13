pro addtwogaussians, x, pp, y, pder

; This is a function adding two gaussians, configured for use with MPCURVEFIT.  
; NOTE: SVDFIT automatically multiplies sigma by sqrt(chisq/DOF), while you 
;       must do this manually w ith MPCURVEFIT.  
; Also, remember to define beforehand weights = replicate(1.,n_elements(r))

z1 = ( x - pp[1] )/pp[2]
z2 = ( x - pp[4] )/pp[5]
y = pp[0]*exp(-z1^2/2) + pp[3]*exp(-z2^2/2)

pder = [ [exp(-z1^2/2)], $
         [pp[0]*z1*exp(-z1^2/2)/pp[2]], $
         [pp[0]*z1*exp(-z1^2/2)*(x-pp[1])/pp[2]^2], $
         [exp(-z2^2/2)], $
         [pp[3]*z2*exp(-z2^2/2)/pp[5]], $
         [pp[3]*z2*exp(-z2^2/2)*(x-pp[4])/pp[5]^2] ]

end
