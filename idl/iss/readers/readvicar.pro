;
; Copyright (c) 1994, Joel Plutchak and Brown University.  Use for any
;   purpose expressly permitted, provided this notice and the modification
;   history below remain intact, and any modifications are noted in the
;   modification history.
;
;   This function was written primarily to read Galileo SSI images, and
;   was written to run under Solaris.   Please let me know if you find
;   any errors in this code, or if you extend the functionality or
;   portability of it.
;                                -Joel Plutchak <Joel_Plutchak@Brown.Edu>
;+
; NAME:
;    READVICAR
;
; PURPOSE:
;   Read a 2-D VICAR image file into an IDL array.
;
; CATEGORY:
;   Input/Output
;
; CALLING SEQUENCE:
;   Result = READVICAR( Filename, /SWAP, /SILENT )
;
; INPUTS:
;  Filename:  A string containing the name of the VICAR image file.
;
; KEYWORDS:
;  SWAP:      If set, swap bytes (only valid for HALF and FULL images).
;  SILENT:    If set, no info messagers
;  HEADER:    variable to return VICAR header as 2xN array of strings in
;
; RETURN VALUE:
;  An array of the appropriate size and type is returned.  On error,
;  a zero is returned.
;
; RESTRICTIONS:
;   Only handles BSQ (band sequential) single-band files.
;   Only handles BYTE type files (not HALF, FULL, REAL, or COMPLEX).
;   This module was only tested on Galileo SSI EDRs and a few other
;      miscellaneous VICAR images, and only running under Solaris 2.3.
;
; EXAMPLE:
;   Read the file GASPRA.IMG in the current directory into the array
;   named GASPRA by entering:
;
;      GASPRA = READVICAR( "GASPRA.IMG" )
;
;   Read the 16-bit-per-pixel file GASPRA.IMG in the current directory
;   into the array named SSI_EDR, swapping bytes, by entering:
;
;      SSI_EDR = READVICAR( "s0202551400.sos", /SWAP )
;
;  To view the image, use commands such as
;
;      TV, GASPRA, /ORDER
;      TVSCL, SSI_EDR, /ORDER
;
;
; MODIFICATION HISTORY:
;
;   08-May-2001 joseph       read label entries at end of file (if there are any)
;   12-Dec-2000 joseph       added HEADER keyword
;   18-Jun-1996 carcich      read_vicar -> readvicar
;   18-Jun-1996 carcich      added some binary conversion functionality
;   18-Jun-1996 carcich      added /SILENT
;   01-Dec-1994 plutchak     Original version, rewritten completely in IDL from my
;                            PV-Wave/C version.
;-


FUNCTION READVICAR, FILE, SWAP=swap, SILENT=silent, HEADER=HEADER
  ;;common lun

  on_error,2                    ; return to caller on error
   
  ;;
  ;; get unit, open file, etc.
  ;;
  lun=-1
  openr, lun, file, error=i, /GET_LUN, /BLOCK
  if (i lt 0) then begin        ; can't open file
    if (lun gt 0) then free_lun, lun
    lun = -1
    message, 'Can''t open file ' + file
  endif
  ;;
  ;; See if it looks like a VICAR file, and read the VICAR label
  ;;
  vlbl = bytarr(16)
  readu, lun, vlbl

  lbl = string(vlbl(0:6))       ; make sure it's a VICAR file

  if (lbl ne 'LBLSIZE') then begin
    message, 'READVICAR: file ' + file + ' is not a VICAR file.'
    free_lun, lun
    return, 0
  endif

  lbl = string(vlbl(8:15))

  lblsiz = 0 + string(vlbl(8:15))
  if (lblsiz le 0) then begin
    message, 'READVICAR: file ' + file + ' has bad labelsize.'
    free_lun, lun
    return, 0
  endif

  lblrec = bytarr(lblsiz)

  point_lun, lun, 0
  readu, lun, lblrec
  ;;print, 'READVICAR: label is ' + string(lblrec)
  ;;
  ;; Parse the label, looking for integer fields NL, NS, NBB, and NLB; and string
  ;; fields TYPE ('IMAGE'), ORG ('BSQ'), FORMAT ('BYTE','HALF','FULL','REAL'),
  ;; INTFMT ('HIGH','LOW'), and REALFMT ('VAX','IEEE').
  nl = -1                       ; some useful default values
  ns = -1
  nb = 1
  nlb = 0
  nbb = 0
  eol = 0

  startk = 0

  ;;btcarcich 12.March, 1996

  realfmt='unknown'
  intfmt = 'unknown'

  header = ['']                 ; initial blank
  while (startk lt strlen(lblrec)) do begin
    ;; find next keyword and value
    endk = strpos( lblrec, '=', startk ) - 1
    if (endk lt 0) then begin
      ;; print, 'End of label found'
      goto, readimage           ; for lack of a 'break' statement
    endif
   
    ;; key on = for keyword, and on ( and ' for value
    startv = endk+2
    if (strmid(lblrec,startv,1) eq "'") then begin
      endv = strpos( lblrec, "'", startv+1 )
    endif else begin
      if (strmid(lblrec,startv,1) eq '(') then begin
        endv = strpos( lblrec, ')', startv+1 )
      endif else begin
        endv = strpos( lblrec, ' ', startv+1 )
      endelse
    endelse

    ;; if a useful label item, save value in variable
    keyword = strcompress(strtrim(strmid(lblrec,startk,endk-startk+1),2))
    value = strcompress(strtrim(strmid(lblrec,startv,endv-startv+1),2))
    case keyword of
      "NL": nl = fix(value)
      "NS": ns = fix(value)
      "NB": nb = fix(value)
      "NLB": nlb = fix(value)
      "NBB": nbb = fix(value)
      "FORMAT": format = value
      "ORG": org = value
      "TYPE": type = value
      "REALFMT": realfmt = value
      "INTFMT": intfmt = value
      "EOL": eol=fix(value)
      else : ;;print, 'KV: ' + keyword+'='+value
    endcase
    header = [header,keyword,value]

    startk = endv + 1
  endwhile
  

readimage:
  ;;
  ;; do some rudimentary sanity checks
  ;;
  if (type ne "'IMAGE'") then begin
    message, 'READVICAR: unsupported file type '+type
    free_lun, lun
    return, 0
  endif

  if (org ne "'BSQ'") then begin
    message, 'READVICAR: unsupported image organization '+org
    free_lun, lun
    return, 0
  endif

  if ((ns le 0) or (nl le 0)) then begin
    message, 'READVICAR: illegal image dimensions '+strtrim(nl,2)+'x'+strtrim(ns,2)
    free_lun, lun
    return, 0
  endif
  ;;
  ;; read the image data into an array, first figuring out details
  ;;
  if (format eq "'BYTE'") then begin ; get pixel size, in bytes
    pixsize = 1
  endif else begin
    if (format eq "'HALF'") then begin
      pixsize = 2
    endif else begin
      pixsize = 4
    endelse
  endelse

  recsize = (ns * pixsize) + nbb ; compute record size, in bytes
  if (nlb gt 0) then begin      ; skip binary labels, if any
    point_lun, lun,  lblsiz + (recsize * nlb)
  endif

  case format of                ; allocate 2-D array of proper size and type
    "'BYTE'": image = bytarr(ns,nl)
    "'HALF'": image = intarr(ns,nl)
    "'WORD'": image = intarr(ns,nl)
    "'FULL'": image = longarr(ns,nl)
    "'LONG'": image = longarr(ns,nl)
    "'REAL'": image = fltarr(ns,nl)
    ELSE: message, 'READVICAR: unknown pixel format ' + format
  endcase

  if not keyword_set( silent) then begin
    print, 'READVICAR: File '+file+' is a '+strtrim(nl,2)+'x'+strtrim(ns,2) $
     +' '+format+' file, with a label size'
    print, 'of '+strtrim(lblsiz,2)+', '+strtrim(nlb,2) $
     +' binary labels and '+strtrim(nbb,2) $
     +' binary bytes (binary labels ignored).'
  endif

  on_ioerror, ioerr
  if (nbb eq 0) then begin      ; no binary record prefix bytes
    readu, lun, image
  endif else begin              ; record-by-record read to skip binary prefixes
    case format of              ; allocate 2-D array of proper size and type
      "'BYTE'": line = bytarr(recsize)
      "'HALF'": line = intarr(recsize/2)
      "'WORD'": line = intarr(recsize/2)
      "'FULL'": line = longarr(recsize/4)
      "'LONG'": line = longarr(recsize/4)
      "'REAL'": line = fltarr(recsize/4)
      ELSE: message, 'READVICAR: unknown pixel format ' + format
    endcase
 
    for linenum = 0, nl-1, 1 do begin ;for each line
      readu, lun, line          ;fill line buffer
      image(0,linenum) = line((nbb/pixsize):*) ;copy the good stuff
    endfor
  endelse


  ;; see if there is more of the vicar label at the end of the file (JJ)
  if (eol) then begin
    point_lun, -lun, image_end
  
    ;;
    ;; read the end label size
    ;;
    vlbl = bytarr(16)
    readu, lun, vlbl

    lbl = string(vlbl(0:6))     ; make sure it's valid

    if (lbl eq 'LBLSIZE') then begin

      lbl = string(vlbl(8:15))

      lblsiz = 0 + string(vlbl(8:15))
      if (lblsiz gt 0) then begin

        lblrec = bytarr(lblsiz)

        point_lun, lun, image_end
        readu, lun, lblrec

        startk = 0
        while (startk lt strlen(lblrec)) do begin
          ;; find next keyword and value
          endk = strpos( lblrec, '=', startk ) - 1
          if (endk lt 0) then begin
            ;; print, 'End of label found'
            goto, eol_done     ; for lack of a 'break' statement
          endif
   
          ;; key on = for keyword, and on ( and ' for value
          startv = endk+2
          if (strmid(lblrec,startv,1) eq "'") then begin
            endv = strpos( lblrec, "'", startv+1 )
          endif else begin
            if (strmid(lblrec,startv,1) eq '(') then begin
              endv = strpos( lblrec, ')', startv+1 )
            endif else begin
              endv = strpos( lblrec, ' ', startv+1 )
            endelse
          endelse

          keyword = strcompress(strtrim(strmid(lblrec,startk,endk-startk+1),2))
          value = strcompress(strtrim(strmid(lblrec,startv,endv-startv+1),2))
          ;; append to header
          header = [header,keyword,value]

          startk = endv + 1
        endwhile
      endif
    endif
  endif

eol_done:
  ;; chop off the initial blank
  ;; and reformat into 2 columns
  h_all = n_elements(header) - 1
  h_num = h_all / 2
  header = reform(header[1:h_all],2,h_num)


  close, lun
  free_lun, lun

  ;;
  ;;  Attempt pixel format conversion, if necessary.  This should really figure
  ;;  out what is necessary given the image format and the host's preferences,
  ;;  rather than rely on a knowledgable user.
  ;;
  ;; carcich 1996/June
  ;; more options:
  ;; - /SWAP dominates!
  ;; - if not /SWAP:
  ;;   - IEEE_TO_HOST if REALFMT="'IEEE'"
  ;;   - CONV_VAX_UNIX if REALFMT="'VAX'" & !VERSION.OS_FAMILY="unix"
  ;;
  ;;
  if (keyword_set(swap)) then begin
    case format of
      "'BYTE'":  ;; print, 'Format is BYTE, swapping makes no sense.  No conversion done.'
      "'HALF'": BYTEORDER, image, /SSWAP
      "'WORD'": BYTEORDER, image, /SSWAP
      "'FULL'": BYTEORDER, image, /LSWAP
      "'LONG'": BYTEORDER, image, /LSWAP
      "'REAL'": print, 'SWAP not supported for type REAL. No swap done.'
    endcase
  endif else begin
    ;;
    ;; ieee to local test
    ;;
    if ( realfmt eq "'IEEE'" ) then begin
      ieee_to_host, image
    endif else begin
      if ( not ( !VERSION.OS_FAMILY eq "vms" ) and $
           ( REALFMT eq "'VAX'" ) ) then begin
        image = conv_vax_unix( image )
      endif
    endelse
  endelse

  return, image

  ;;
  ;; Used for debugging
  ;;
ioerr:
  bleh = fstat(lun)
  print, 'IO Error==========='
  print, '  file:  ', bleh.name
  print, '  read:  ', bleh.read
  print, '  count: ', bleh.transfer_count
  print, '  pos:   ', bleh.cur_ptr
  print, '  size:  ', bleh.size
  print, '  recl:  ', bleh.rec_len
  print, '==================='

  close, lun
  free_lun, lun
  return, image

end

