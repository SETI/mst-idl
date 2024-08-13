Function M_imaginary, x

;+
; NAME:
;	M_IMAGINARY
; VERSION:
;	3.0
; PURPOSE:
;	Returns imaginary values.
; CATEGORY:
;	Mathematical, general.
; CALLING SEQUENCE:
;	Result = M_IMAGINARY (X)
; INPUTS:
;    X
;	Numerical, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the imaginary part of the input, i.e. 0 if the input is real
;	the imaginary part in FLOAT format for COMPLEX and the imaginary part 
;	in DOUBLE format for DCOMPLEX.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Straightforward.  Calling FPU_FIX and ISNUM from MIDL.
; MODIFICATION HISTORY:
;	Created 5-MAY-1996 by Mati Meron.
;	Modified 15-SEP-1998 by Mati Meron.  Underflow filtering added.
;-

    on_error, 1
    if not Isnum(x) then message, 'Not numeric!"

    if Isnum(x, /complex) then return, FPU_fix(imaginary(x)) $
    else if n_elements(x) gt 1 then return, make_array(size = size(x)) $
    else return, 0b*x
end
