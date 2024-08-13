;=============================================================================
;+
; NAME:
;       v_matrix_multiply
;
;
; PURPOSE:
;       Multiplies the (3,3,N) matrix, m, with the (N,3) column vectors ,v.
;
;
; CATEGORY:
;       UTIL/V
;
;
; CALLING SEQUENCE:
;       result = v_matrix_multiply(m, v)
;
;
; ARGUMENTS:
;  INPUT:
;               m:	An array of N 3x3 matricies
;
;               v:      An array of N column vectors
;
;
;  OUTPUT:
;       NONE
;
; KEYWORDS:
;       NONE
;
; RETURN:
;	Column vectors (N,3).
;
;
; RESTRICTIONS:
;       NONE
;
; STATUS:
;       Completed.
;
;
; MODIFICATION HISTORY:
;	Written by: 	Tiscareno, 7/2001
;
;-
;=============================================================================
function v_matrix_multiply, m, v

out = [ [ m[0,0,*]*v[*,0] + m[1,0,*]*v[*,1] + m[2,0,*]*v[*,2] ], $
        [ m[0,1,*]*v[*,0] + m[1,1,*]*v[*,1] + m[2,1,*]*v[*,2] ], $
        [ m[0,2,*]*v[*,0] + m[1,2,*]*v[*,1] + m[2,2,*]*v[*,2] ] ]

if (size(v))[1] gt 1 then begin
  return, transpose(out,[2,1])
endif else begin
  return, out
endelse

end
