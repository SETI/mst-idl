function amoeba_test_func, p0

common funct_xy, x, y
f = p0[0] + p0[1]*x
return, max(abs( f - y ))

end

pro amoeba_test

; Define a common block to transfer the x and y values into the function.
common funct_xy, x, y

; Define x values and y values, y = 6 + 11*x
x = findgen(100)
y = 6 + 11*x

; Define preliminary guess with bogus values
p0 = [ -9, 24 ]
; Amoeba will not look further from p0[j] than scale[j] for the minimum value.
scale = [ 1e2, 1e2 ]

; Now call Amoeba.  Set the fractional tolerance to 1e-5.
p = amoeba( 1e-5, scale=scale, p0=p0, function_value=fval, $
	function_name='amoeba_test_func' )

; The output values p should be the correct coefficients ( 6 and 11 ).
print, p
; Output fval[0] gives the minimum deviation of the fitted function.
; Output fval[j] gives the deviation of the fitted function at p[j-1].
print, fval 

end
