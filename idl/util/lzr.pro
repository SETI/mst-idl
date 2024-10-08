pro   lzr,fname,port=port,halfsize=halfsize,aspect=aspect,hland=hland,hport=hport,qland=qland,qport=qport,psquare=psquare,encapsulate=encapsulate,specifyxy=specifyxy
;  this routine prepares a postscript file for printing out plots
;  if the string fname is not given it will prompt the user for a
;  filename. The naming convention is that the filename will be
;  fname.plt 
;  Now protected against running if already run without using clzr.pro
;  3/05/92 CMJ
common device,old

par=n_params()
if (par lt 1) then begin
   fname=''
   print, 'Enter the desired filename'
   read,fname 
endif

if !d.name eq 'PS' then begin
   print,'You have not yet run CLZR on a previously opened plot file'
   print,'Please run CLZR first'
   retall
endif
old=!d.name
set_plot,'ps'
; Default is ysize = 17.78 cm = 7 in, and xsize = 24.13 cm = 9.5 in
device,/land
; User-defined aspect ratio
if keyword_set(aspect) then device,/land,/inches,xsize=9.5,ysize=aspect*9.5,$
                                   yoffset=11.0,xoffset=7.0*(1-aspect)-1.0
; User-defined axes
if keyword_set(specifyxy) then begin
  if n_elements(specifyxy) ne 2 then begin
    print, 'specifyxy must have two elements'
  endif else begin
    device,/land,/inches,xsize=specifyxy[0],ysize=specifyxy[1],$
           yoffset=11.0,xoffset=0
  endelse
endif
; Portrait, changing aspect ratio to fill page
if keyword_set(port) then device,/port,/inches,ysize=9.5,yoffset=1.0
; Portrait, with aspect ratio of 1
if keyword_set(psquare) then device,/port,/inches,ysize=7,yoffset=1.0
; Portrait, aspect ratio unchanged (approximately half-page)
if keyword_set(halfsize) then device,/port
; Half page, no margins, landscape
if keyword_set(hland) then device,/land,/inches,ysize=8.5,xsize=5.5,$
                                  yoffset=11.0,xoffset=0
; Half page, no margins, portrait (change aspect ratio)
if keyword_set(hport) then device,/port,/inches,ysize=5.5,xsize=8.5,$
                                  yoffset=0,xoffset=0
; Quarter page, no margins, landscape
if keyword_set(qland) then device,/land,/inches,ysize=4.25,xsize=5.5,$
                                  yoffset=11.0,xoffset=0
; Quarter page, no margins, portrait (change aspect ratio)
if keyword_set(qport) then device,/port,/inches,ysize=5.5,xsize=4.25,$
                                  yoffset=0,xoffset=0
if keyword_set(encapsulate) then begin
  device, /encapsulate
  suffix='.eps'
endif else suffix='.ps'
device,file=fname+suffix

end

