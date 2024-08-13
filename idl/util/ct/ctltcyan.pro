;========================================================================
;+
; NAME:
;	ctltcyan
;
; PURPOSE:
;	To allocate/return the color light cyan (24-bit only).
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
function ctltcyan

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 7
  24		 : return, 16777128 ;bin_to_dec('111111111111111110101000')
  else		 : return, 0
 endcase
end
;========================================================================
