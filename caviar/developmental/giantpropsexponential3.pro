pro giantpropsexponential3, x, pp, y, pder, offsetlon=offsetlon

; This is a function tailored for propeller_orbit3.pro, 
; configured for use with MPCURVEFIT.  
; pp[0] is the initial value
; pp[1] is the final value
; pp[2] is the characteristic timescale
; pp[3] is the initial time
; NOTE: SVDFIT automatically multiplies sigma by sqrt(chisq/DOF), while you 
;       must do this manually w ith MPCURVEFIT.  
; Also, remember to define beforehand weights = replicate(1.,n_elements(r))
; or, if you have error bars and used measure_errors in SVDFIT, 
; weights = 1.0d0/errbars^2

y = pp[1] - (pp[1]-pp[0])*exp(-2*!dpi/pp[2]*(x-pp[3]))
pder = [ [exp(-2*!dpi/pp[2]*(x-pp[3]))], [1-exp(-2*!dpi/pp[2]*(x-pp[3]))], $
         [-2*!dpi*(x-pp[3])/pp[2]^2*(y-pp[1])], [-2*!dpi/pp[2]*(y-pp[1])] ]

;pp3 = min(x)-(max(x)-min(x))/10
;y = pp[1] - (pp[1]-pp[0])*exp(-2*!dpi/pp[2]*(x-pp3))
;pder = [ [exp(-2*!dpi/pp[2]*(x-pp3))], [1-exp(-2*!dpi/pp[2]*(x-pp3))], $
;         [-2*!dpi*(x-pp3)/pp[2]^2*(y-pp[1])], [replicate(0,n_elements(x))] ]

;y = pp[1] - (pp[1]-pp[0])*exp(-2*!dpi/pp[2]*(x-pp[3]))
;pder = [ [exp(-2*!dpi/pp[2]*(x-pp[3]))], [1-exp(-2*!dpi/pp[2]*(x-pp[3]))], $
;         [-2*!dpi*(x-pp[3])/pp[2]^2*(y-pp[1])], [-2*!dpi/pp[2]*(y-pp[1])] ]
;foo = where( x lt pp[3], count )
;if count gt 0 then begin
;  pder[foo,0:2] = 0
;  pder[foo,3] = pp[0] - y[foo]
;  y[foo] = pp[0]
;endif

end
