;=============================================================================
;+
; NAME:
;       v_unit
;
;
; PURPOSE:
;       Returns unit vectors in the directions given by v.
;
;
; CATEGORY:
;       UTIL/V
;
;
; CALLING SEQUENCE:
;       result = v_unit(v)
;
;
; ARGUMENTS:
;  INPUT:
;               v:      An array of n column vectors
;
;  OUTPUT:
;       NONE
;
; RETURN:
;       An array of n unit vectors
;
;
; STATUS:
;       Completed.
;
;
; MODIFICATION HISTORY:
;       Written by:     Spitale
;
;-
;=============================================================================
function v_unit, v
; return, v/(sqrt(total(v*v, 2))#make_array(3, val=1))

 vmag = sqrt(total(v*v, 2))
 u = v
 u[*,0,*] = v[*,0,*]/vmag
 u[*,1,*] = v[*,1,*]/vmag
 u[*,2,*] = v[*,2,*]/vmag
 return, u
end
;===========================================================================
