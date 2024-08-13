;========================================================================
;+
; NAME:
;       ctgray
;
; PURPOSE:
;       To allocate/return the color gray (returns white in 8-bit).
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctgray()
;
; RETURN:
;       The lookup table or true color value for gray
;
;-
;========================================================================
function ctgray

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 1
  24		 : return, 8421504
  else		 : return, 0
 endcase
end
;========================================================================
