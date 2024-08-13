;========================================================================
;+
; NAME:
;       ctltorange
;
; PURPOSE:
;       To allocate/return the color light orange (returns red in 8-bit).
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctltorange()
;
; RETURN:
;       The lookup table or true color value for light orange
;
;-
;========================================================================
function ctltorange

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 6
  24		 : return, 5548287 ;bin_to_dec('10101001010100011111111')
  else		 : return, 0
 endcase
end
;========================================================================
