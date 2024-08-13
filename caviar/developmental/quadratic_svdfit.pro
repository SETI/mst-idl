function quadratic_svdfit, x, m

; This is a pure quadratic function, configured for use with SVDFIT.  
; I have found that:
; fit = SVDFIT( r, v, 3, function_name='quadratic_svdfit' )
; gives the same result as 
; fit = SVDFIT( r, v, 3 )

return, [ [1.0], [x], [x^2] ]

end
