;========================================================================
;+
; NAME:
;       ctltgray
;
; PURPOSE:
;       To allocate/return the color light gray (24-bit only).
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctltgray()
;
; RETURN:
;       The lookup table or true color value for light gray
;
;-
;========================================================================
function ctltgray

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 1
  24		 : return, 11053224 ;bin_to_dec('101010001010100011111111')
  else		 : return, 0
 endcase
end
;========================================================================
