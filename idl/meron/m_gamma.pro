Function M_gamma, x

;+
; NAME:
;	M_GAMMA
; VERSION:
;	3.0
; PURPOSE:
;	Calculates the gamma function.  Replacement for the IDL GAMMA function 
;	which accepts only real input.
; CATEGORY:
;	Mathematical, general.
; CALLING SEQUENCE:
;	Result = GAMMA (X)
; INPUTS:
;    X
;	Numeric, otherwise arbitrary.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;	None.
; OUTPUTS:
;	Returns the gamma function of X.  Output type is same as input (but no
;	lower than FLOAT), form is same as input.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	The real part of X should be greater than 0.
; PROCEDURE:
;	Calls M_LNGAMMA.
; MODIFICATION HISTORY:
;	Created 30-MAR-1996 by Mati Meron.
;-

    return, exp(M_lngamma(x))
end
