function sine5_svdfit, x, m

; This is a pure sine function, plus a linear function, 
; configured for use with SVDFIT.  
; Compare sine5.pro, which is configured for MPCURVEFIT. 

;y = m[0] * sin( m[1]*x + m[2] ) + m[3] + m[4]*x
help, x
pder = [ [ sin( m[1]*x + m[2] ) ], $
         [ m[0] * x * cos( m[1]*x + m[2] ) ], $
         [ m[0] * cos( m[1]*x + m[2] ) ], $
         [ replicate( 1, n_elements(x) ) ], $
         [ x ] ]
return, pder

end
