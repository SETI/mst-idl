;========================================================================
;+
; NAME:
;       ctltblue
;
; PURPOSE:
;       To allocate/return the color light blue (24-bit only).
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctltblue()
;
; RETURN:
;       The lookup table or true color value for blue
;
;-
;=======================================================================
function ctltblue

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 4
  24		 : return, 16754856 ;bin_to_dec('111111111010100010101000')
  else		 : return, 0
 endcase
end
;========================================================================
