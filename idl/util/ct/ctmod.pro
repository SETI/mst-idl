;========================================================================
;+
; NAME:
;       ctmod
;
; PURPOSE:
;       To get display visual type (8 or 24 bit) and reserve a number of
;       colors for plotting in the lookup table (for 8 bit).
;
;
; CATEGORY:
;       UTIL/CT
;
;
; CALLING SEQUENCE:
;       cdmod, visual=visual, top=top

; ARGUMENTS:
;  INPUT:
;       NONE
;
;  OUTPUT:
;    visual:  Number of planes in idl image device.
;
;       top:  New top of lookup table available to image display.
;
;-
;========================================================================
pro ctmod, visual=visual, top=top

 ;----------------------
 ; detect visual type
 ;----------------------
 visual = round(alog(!d.n_colors)/alog(2))

 if(visual LT 8 AND visual GT 1) then visual=8

 if(visual GT 24) then message, 'Unsupported visual.'

 if(visual NE 8) then return


 ;-------------------
 ; get current table
 ;-------------------
 catch, errno					; catch graphics device error
 if(errno NE 0) then $
  begin
   if(errno EQ -366) then return
  end

 tvlct, r, g, b, /get

 ;----------------------------------------
 ; compress table and add plotting colors
 ;----------------------------------------
 n = n_elements(r)

 n1 = n - 7
 top = n1-2

 sub = lindgen(n1)*(n/n1)
 r1 = [r[sub], 0,   255, 0,    0, 255, 255, 255]
 g1 = [g[sub], 255, 0,   255,  0, 255, 0,   255]
 b1 = [b[sub], 255, 0,   0,  255, 0,   255, 255]

 ;-------------------
 ; load new table
 ;-------------------
 tvlct, r1, g1, b1
end
;========================================================================
