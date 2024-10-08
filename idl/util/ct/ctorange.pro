;========================================================================
;+
; NAME:
;       ctorange
;
; PURPOSE:
;       To allocate/return the color orange (returns red in 8-bit).
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctorange()
;
; RETURN:
;       The lookup table or true color value for orange
;
;-
;========================================================================
function ctorange

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 6
  24		 : return, 33023 ;bin_to_dec('1000000011111111')
  else		 : return, 0
 endcase
end
;========================================================================
