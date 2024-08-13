pro giantpropsexponential, x, pp, y, pder, offsetlon=offsetlon

; This is a function tailored for propeller_orbit3.pro, 
; configured for use with MPCURVEFIT.  
; offsetlon is the initial value
; offsetlon+pp[0] is the final value
; 2*!pi/pp[1] is the characteristic timescale
; pp[2] is the initial time
; NOTE: SVDFIT automatically multiplies sigma by sqrt(chisq/DOF), while you 
;       must do this manually w ith MPCURVEFIT.  
; Also, remember to define beforehand weights = replicate(1.,n_elements(r))
; or, if you have error bars and used measure_errors in SVDFIT, 
; weights = 1.0d0/errbars^2

y = pp[0]*( 1 - exp(-pp[1]*(x-pp[2])) )
if keyword_set(offsetlon) then y = y + offsetlon

pder = [ [1-exp(-pp[1]*(x-pp[2]))], [pp[0]*pp[1]*exp(-pp[1]*(x-pp[2]))], $
         [-pp[0]*pp[1]*pp[2]*exp(-pp[1]*(x-pp[2]))] ]

end
