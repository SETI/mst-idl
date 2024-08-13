;========================================================================
;+
; NAME:
;       ctwhite
;
; PURPOSE:
;       To allocate/return the color white.
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctwhite()
;
; RETURN:
;       The lookup table or true color value for white
;
;-
;========================================================================
function ctwhite

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 1
  24		 : return, 16777215
  else		 : return, 0
 endcase
end
;========================================================================
