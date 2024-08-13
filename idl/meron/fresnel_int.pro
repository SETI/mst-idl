Function Fresnel_int, x, sine = sin, complementary = comp

;+
; NAME:
;	FRESNEL_INT
; VERSION:
;	3.0
; PURPOSE:
;	Calculates the Fresnel Integrals, C(x) or S(X).
; CATEGORY:
;	Mathematical function (general).
; CALLING SEQUENCE:
;	Result = FRESNEL_INT (X [, keywords])
; INPUTS:
;    X
;	Numeric, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    /SINE
;	Switch.  If set, the Fresnel sine integral, S(X), is returned.
;    /COMPLEMENTARY
;	Switch.  If set, 1/2 - C(X) (or S(X)) is returned.
; OUTPUTS:
;	Returns the Fresnel Cosine integral C(X) or, if /COMPLEMENTARY is set,
;	the sine integral S(X).  If /COMPLEMENTARY is set, the output is 
;	1/2 (the value at infinity) - C(X) (or S(X)).
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	While the function is defined also for complex values of X, large 
;	complex values may cause calculational instabilities.  A warning is 
;	issued in this case.
; PROCEDURE:
;	Uses the function M_ERRORF from MIDL, as well as CAST and TYPE, also
;	from MIDL.
; MODIFICATION HISTORY:
;	Created 20-MAR-1996 by Mati Meron.
;-

    on_error, 1
    typ = Type(x)
    cof = 2*sqrt(!dpi)
    opi = 0.25d*dcomplex(1, 1)
    oni = 0.25d*dcomplex(1,-1)

    if keyword_set(sin) $
	then res = oni*M_errorf(cof*oni*x, comp = comp) + $
		opi*M_errorf(cof*opi*x, comp = comp) $
	else res = opi*M_errorf(cof*oni*x, comp = comp) + $
		oni*M_errorf(cof*opi*x, comp = comp)

    return, Cast(res,4,typ,/fix)
end
