pro lorentzian_cosine, x, pp, y, pder

; This is a repeating lorentzian function, plus a cosine function, 
; configured for use with MPCURVEFIT.  
; The lorentzian is confined to be centered with a negative peak at 
; phase 0 and a positive peak at phase pi. 
; NOTE: SVDFIT automatically multiplies sigma by sqrt(chisq/DOF), while you 
;       must do this manually w ith MPCURVEFIT.  
; Also, remember to define beforehand weights = replicate(1.,n_elements(r))

; First the cosine curve (modeled from sine3.pro)
y = pp[0] * cos( pp[1]*x + pp[2] )
pder = [ [ cos( pp[1]*x + pp[2] ) ], $
         [ - pp[0] * x * sin( pp[1]*x + pp[2] ) ], $
         [ - pp[0] * sin( pp[1]*x + pp[2] ) ], $
         [ replicate(0,n_elements(x)) ], $
         [ replicate(0,n_elements(x)) ] ]

nmin = floor( (pp[1]*min(x)-pp[2]) / !dpi )
nmax = ceil( (pp[1]*max(x)-pp[2]) / !dpi )
for n=nmin,nmax do begin
  u = ( x - (n*!dpi+pp[2])/pp[1] )/pp[4]
  y = y - pp[3]*(-1)^n/(u^2+1)
  pder = [ [ pder[*,0] ], $
           [ pder[*,1] + 2*pp[3]*(-1)^n/(u^2+1)^2*u*(n*!dpi+pp[2])/pp[1]^2 ], $
           [ pder[*,2] - 2*pp[3]*(-1)^n/(u^2+1)^2*u/pp[1]/pp[4] ], $
           [ pder[*,3] - (-1)^n/(u^2+1) ], $
           [ pder[*,4] - 2*pp[3]*(-1)^n/(u^2+1)^2*u^2/pp[4] ] ]
endfor

end
