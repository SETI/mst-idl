pro lorentzian_quadratic, x, pp, y, pder

; This is a lorentzian function, plus a quadratic function, 
; configured for use with MPCURVEFIT.  
; I have found that:
; fit = CURVEFIT( r, v, weights, pp, function_name='lorentzian_quadratic' )
; gives the same result as 
; fit = MPFITPEAK( r, v, pp, /lorentzian, nterms=6 )
; NOTE: SVDFIT automatically multiplies sigma by sqrt(chisq/DOF), while you 
;       must do this manually w ith MPCURVEFIT.  
; Also, remember to define beforehand weights = replicate(1.,n_elements(r))

wid = abs(pp[2]) > 1e-20
u = (x-pp[1])/wid
y = pp[0] / (u^2+1)
pder = [ [ 1 / (u^2+1) ], $
         [ 2 / (u^2+1)^2 * u/wid ], $
         [ 1 / (u^2+1)^2 * u^2/wid * abs(pp[2])/pp[2] ] ]
if n_elements(pp) ge 4 then begin
  y = y + pp[3]
  pder = [ [ pder ], [ replicate(1,n_elements(x)) ] ]
endif
if n_elements(pp) ge 5 then begin
  y = y + pp[4]*x
  pder = [ [ pder ], [ x ] ]
endif
if n_elements(pp) ge 6 then begin
  y = y + pp[5]*x^2
  pder = [ [ pder ], [ x^2 ] ]
endif

end
