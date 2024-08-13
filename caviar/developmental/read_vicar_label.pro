

;=============================================================================
;+
; $Id: read_vicar_label.pro,v 1.1 2008/08/27 18:04:32 nathan Exp $
; read_vicar_label
;
; PURPOSE
;`
;  Reads a vicar data file.
;
;'
; CALLING SEQUENCE :
;
;       label=read_vicar_label(filename)
;
;
; ARGUMENTS
;  INPUT : filename - String giving the name of the file to be read.
;
;
; KEYWORDS 
;
;          silent - If set, no messages are printed.
;
;
;  OUTPUT : status - If no errors occur, status will be zero, otherwise
;                    it will be a string giving an error message.
;
;
;
; RETURN : The VICAR label read from the file.
;
;
; RESTRICTIONS : This program only works with band-sequential data and
;                does not recognize EOF labels.
;
;
;
; KNOWN BUGS : NONE
;
;
;
; ORIGINAL AUTHOR : J. Spitale ; 10/95 (read_vicar.pro)
;
;-
; UPDATE HISTORY : 
; $Log: read_vicar_label.pro,v $
; Revision 1.1  2008/08/27 18:04:32  nathan
; added cvs keywords
;
;   Jeffrey.R.Hall, 6/2003
;			read_vicar.pro was stripped down to create read_vicar_label.pro.
;
;=============================================================================

;===========================================================================
; read_vicar_label
;
;===========================================================================
function read_vicar_label, filename, status=status, silent=silent

 status=0

;----------------open file------------------

 openr, unit, filename, /get_lun, error=error
 if(error NE 0) then $
  begin
   status=!err_string
   if(NOT keyword_set(silent)) then message, status
   return, 0
  end


;---------------read label size----------------

 records=assoc(unit, bytarr(30, /nozero))
 record=records(0)
 str=string(record)

 label_nbytes=vicgetpar(str, 'LBLSIZE', status=status)
 if(keyword_set(status)) then $
  begin
   if(NOT keyword_set(silent)) then message, status
   return, 0
  end


;-----------------get label-------------------

 label_records=assoc(unit, bytarr(label_nbytes, /nozero))
 label=string(label_records(0))


;------------------clean up-------------------

 close, unit
 free_lun, unit


 return, label
end
;===========================================================================
