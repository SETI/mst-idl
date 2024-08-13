pro gaussian1, x, a, f, pder

; Compute a gaussian and its derivatives, given abscissa x and constants a. 
; Format compliant with curvefit.pro

z = ( x - a[1] )/a[2]
f = a[0] * exp( -z^2 / 2 )
pder = [ [f/a[0]], [z*f/a[2]], [z*f*(x-a[1])/a[2]^2] ]
na = n_elements(a)
if na gt 3 then begin
  f = f + a[3]
  pder = [ [pder], [replicate(1,n_elements(x))] ]
  if na gt 4 then begin
    f = f + a[4]*x
    pder = [ [pder], [x] ]
    if na gt 5 then begin
      f = f + a[5]*x^2
      pder = [ [pder], [x^2] ]
    endif
  endif
endif

end
