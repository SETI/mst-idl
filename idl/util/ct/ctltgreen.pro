;========================================================================
;+
; NAME:
;       ctltgreen
;
; PURPOSE:
;       To allocate/return the color light green (24-bit only).
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctltgreen()
;
; RETURN:
;       The lookup table or true color value for green
;
;-
;========================================================================
function ctltgreen

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 5
  24		 : return, 11075496 ;bin_to_dec('101010001111111110101000')
  else		 : return, 0
 endcase
end
;========================================================================
