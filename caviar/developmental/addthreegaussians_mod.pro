pro addthreegaussians_mod, x, pp, y, pder

; This is a function adding three gaussians, configured for use with MPCURVEFIT.
; NOTE: SVDFIT automatically multiplies sigma by sqrt(chisq/DOF), while you 
;       must do this manually w ith MPCURVEFIT.  
; Also, remember to define beforehand weights = replicate(1.,n_elements(r))
  
; Here we have replaced the usual gaussian constants -- namely height, center,
; and sqrt(variance) -- with height, log-normal center, and volume. 

z1 = sqrt(2*!dpi)*pp[0]*( x - pp[1] )/pp[2] + pp[2]/2/sqrt(2*!dpi)/pp[0]
z2 = sqrt(2*!dpi)*pp[3]*( x - pp[4] )/pp[5] + pp[5]/2/sqrt(2*!dpi)/pp[3]
z3 = sqrt(2*!dpi)*pp[6]*( x - pp[7] )/pp[8] + pp[8]/2/sqrt(2*!dpi)/pp[6]
z1p = sqrt(2*!dpi)*pp[0]*( x - pp[1] )/pp[2] - pp[2]/2/sqrt(2*!dpi)/pp[0]
z2p = sqrt(2*!dpi)*pp[3]*( x - pp[4] )/pp[5] - pp[5]/2/sqrt(2*!dpi)/pp[3]
z3p = sqrt(2*!dpi)*pp[6]*( x - pp[7] )/pp[8] - pp[8]/2/sqrt(2*!dpi)/pp[6]
y = pp[0]*exp(-z1^2/2) + pp[3]*exp(-z2^2/2) + pp[6]*exp(-z3^2/2)

pder = [ [exp(-z1^2/2)*( 1 - z1*z1p )], $
         [exp(-z1^2/2)*pp[0]*z1*sqrt(2*!dpi)*pp[0]/pp[2]], $
         [exp(-z1^2/2)*pp[0]*z1*z1p/pp[2]], $
         [exp(-z2^2/2)*( 1 - z2*z2p )], $
         [exp(-z2^2/2)*pp[3]*z2*sqrt(2*!dpi)*pp[3]/pp[5]], $
         [exp(-z2^2/2)*pp[3]*z2*z2p/pp[5]], $
         [exp(-z3^2/2)*( 1 - z3*z3p )], $
         [exp(-z3^2/2)*pp[6]*z3*sqrt(2*!dpi)*pp[6]/pp[8]], $
         [exp(-z3^2/2)*pp[6]*z3*z3p/pp[8]] ]

end
