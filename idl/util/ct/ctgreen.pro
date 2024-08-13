;========================================================================
;+
; NAME:
;       ctgreen
;
; PURPOSE:
;       To allocate/return the color green.
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctgreen()
;
; RETURN:
;       The lookup table or true color value for green
;
;-
;========================================================================
function ctgreen

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 5
  24		 : return, 65280
  else		 : return, 0
 endcase
end
;========================================================================
