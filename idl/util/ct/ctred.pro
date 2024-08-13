;========================================================================
;+
; NAME:
;       ctred
;
; PURPOSE:
;       To allocate/return the color red.
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctred()
;
; RETURN:
;       The lookup table or true color value for red
;
;-
;========================================================================
function ctred

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 6
  24		 : return, 255
  else		 : return, 0
 endcase
end
;========================================================================
