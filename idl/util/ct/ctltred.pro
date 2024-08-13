;========================================================================
;+
; NAME:
;       ctltred
;
; PURPOSE:
;       To allocate/return the color light red (24-bit only).
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctltred()
;
; RETURN:
;       The lookup table or true color value for light red
;
;-
;========================================================================
function ctltred

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 6
  24		 : return, 11053311 ;bin_to_dec('101010001010100011111111')
  else		 : return, 0
 endcase
end
;========================================================================
