;========================================================================
;+
; NAME:
;       ctblue
;
; PURPOSE:
;       To allocate/return the color blue.
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctblue()
;
; RETURN:
;       The lookup table or true color value for blue
;
;-
;=======================================================================
function ctblue

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 4
  24		 : return, 16711680
  else		 : return, 0
 endcase
end
;========================================================================
