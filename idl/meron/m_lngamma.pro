Function M_lngamma, x

;+
; NAME:
;	M_LNGAMMA
; VERSION:
;	3.0
; PURPOSE:
;	Calculates the natural log of the gamma function.  Replacement for the 
;	IDL LNGAMMA function which accepts only real input.
; CATEGORY:
;	Mathematical function (general).
; CALLING SEQUENCE:
;	Result = M_LNGAMMA (X, A [,EPS ] [,/COMPLEMENTARY ])
; INPUTS:
;    X
;	Numeric, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the natural logarithm of the gamma function of X.  Output type
;	and form are identical to those of the input (but output type is never
;	lower than FLOAT).
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	The real part of X should be positive.
; PROCEDURE:
;	Uses a continued fraction expansion.  Calls CAST, CONFRAC, DEFAULT, 
;	TYPE and TOLER, from MIDL.
; MODIFICATION HISTORY:
;	Created 30-MAR-1996 by Mati Meron.
;-

    on_error, 1

    nex = Default(nex,0d,/dtype)
    a = [1d/12d, 1d/30d, 53d/210d, 195d/371d, 22999d/22737d, $
	29944523d/19733142d, 109535241009d/48264275462d, $
	2.95520928d,3.34489801d,2.40361667d,1.19320621d,0.40123515d,0.07143468d]

    b = dblarr(n_elements(a),2)
    b(*,1) = 1d
    xpo = x + 1d

    res = 0.5d*alog(2d*!dpi) + (xpo - 0.5d)*alog(xpo) - xpo - alog(x) + $
	Confrac(a,b,xpo,eps = Toler(x), /rel)

    return, Cast(res,4,Type(x))
end
