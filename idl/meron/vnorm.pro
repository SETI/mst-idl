Function Vnorm, r

;+
; NAME:
;	VNORM
; VERSION:
;	3.0
; PURPOSE:
;	Finds the norm of a vector.
; CATEGORY:
;	Geometry, General.
; CALLING SEQUENCE:
;	Result = VNORM (R)
; INPUTS:
;    R
;	Vector, numeric, otherwise arbitrary.  Can be complex.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the norm of the vector R.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Calls CAST, FPU_FIX, ISNUM and M_ABS from MIDL.
; MODIFICATION HISTORY:
;	Created 30-JUN-1992 by Mati Meron.
;	Modified and added to MIDL 5-NOV-1997 by Mati Meron.
;	Modified 10-SEP-1998 by Mati Meron.  Underflow filtering added.
;-

    on_error, 1
    if not Isnum(r) then message, 'Input must be numeric!' else $
    return, sqrt(total(FPU_fix(M_abs(Cast(r,4))^2)))
end
