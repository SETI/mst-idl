Function Sorpurge, arr, netlen = nl

;+
; NAME:
;	SORPURGE
; VERSION:
;	3.0
; PURPOSE:
;	Similar to the SORT function, but ignores repeated values of elements.
; CATEGORY:
;	Array Manipulation.
; CALLING SEQUENCE:
;	Result = SORPURGE ( ARR [, keywords])
; INPUTS:
;    ARR
;	The array to be sorted (scalar is also accepted).
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    NETLEN
;	Optional output, see below.
; OUTPUTS:
;	Returns a vector of indices that allow access to all DIFFERENT elements
;	of ARR in ascending order.
; OPTIONAL OUTPUT PARAMETERS:
;    NETLEN
;	The name of a variable to receive the number of elements .
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Uses SORT to order the array, then finds all the elements that differ
;	from their predecessors.
; MODIFICATION HISTORY:
;	Created 15-AUG-1992 by Mati Meron.
;-

    on_error, 1

    nl = n_elements(arr)
    if nl eq 0 then message, 'Variable is not defined!' $
    else if nl eq 1 then sorp = [0l] $
    else begin
	sorp = sort(arr)
	dum = arr(sorp)
	wdum = where((dum(1:nl-1) - dum(0:nl-2)) ne 0, ndif)
	if ndif eq 0 then sorp = [0l] else sorp = sorp([0, 1 + wdum])
	nl = 1 + ndif
    endelse

    return, sorp
end
