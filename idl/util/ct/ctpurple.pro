;========================================================================
;+
; NAME:
;       ctpurple
;
; PURPOSE:
;       To allocate/return the color purple.
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       return = ctpurple()
;
; RETURN:
;       The lookup table or true color value for purple
;
;-
;========================================================================
function ctpurple

 ctmod, visual=visual

 case visual of
  1		 : return, 1
  8		 : return, !d.table_size - 2
  24		 : return, 16711935
  else		 : return, 0
 endcase
end
;========================================================================
