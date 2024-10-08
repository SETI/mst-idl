Function M_real, x

;+
; NAME:
;	M_REAL
; VERSION:
;	3.0
; PURPOSE:
;	Returns real values.
; CATEGORY:
;	Mathematical, general.
; CALLING SEQUENCE:
;	Result = M_REAL (X)
; INPUTS:
;    X
;	Numerical, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the real value of the input, i.e. the input itself if its real
;	the real part in FLOAT format for COMPLEX and the real part in DOUBLE
;	format for DCOMPLEX.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Calling CAST, FPU_FIX and ISNUM from MIDL.
; MODIFICATION HISTORY:
;	Created 5-MAY-1996 by Mati Meron.
;	Modified 15-SEP-1998 by Mati Meron.  Underflow filtering added.
;-

    on_error, 1
    if not Isnum(x) then message, 'Not numeric!"

    if Isnum(x, /complex, typ= xtyp) then return, FPU_fix(Cast(x,4,xtyp/2+ 1)) $
    else return, FPU_fix(x)
end
