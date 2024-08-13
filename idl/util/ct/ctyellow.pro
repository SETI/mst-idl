;========================================================================
;+
; NAME:
;       ctyellow
;
; PURPOSE:
;       To allocate/return the color yellow.
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctyellow()
;
; RETURN:
;       The lookup table or true color value for yellow
;
;-
;========================================================================
function ctyellow

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 3
  24		 : return, 65535
  else		 : return, 0
 endcase
end
;========================================================================
