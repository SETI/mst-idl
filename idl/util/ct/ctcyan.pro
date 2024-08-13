;========================================================================
;+
; NAME:
;	ctcyan
;
; PURPOSE:
;	To allocate/return the color cyan.
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;	return = ctcyan()
;
; RETURN:
;	The lookup table or true color value for cyan
;
;-
;========================================================================
function ctcyan

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 7
  24		 : return, 16776960
  else		 : return, 0
 endcase
end
;========================================================================
