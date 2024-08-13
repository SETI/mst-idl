
savefile = "wisps.sav"

; if there is a file named wisps.sav then we're good else make it
IF (keyword_set(findfile(savefile))) THEN BEGIN
   restore, savefile
ENDIF

FOR numimg = 0, n_elements(filenames)-1 DO BEGIN
   image_name = filenames[numimg]
   PRINT, image_name
   @caviar
   

;.run restore_redge in a way that it can be run
   if keyword_set(linsam) then linsam = '_linsam' else linsam = ''
   period = rstrpos( image_name, '.' )
   filestem = strmid( image_name, 0, period )
   f = findfile( filestem+'.edge*'+linsam )
   if not keyword_set(linsam) then begin
      ; Exclude any existing linsam files
      foo = where( strpos(f,'linsam') eq -1, count )
      if count eq 0 then f='' else f = f[foo]
   endif
   if not keyword_set(f) then begin
      print, 'No saved '+strmid(linsam,1,50)+'edgefits found for filestem '+filestem
   endif else begin
      nf = n_elements(f)
      if nf eq 1 then savefile = f[0] else begin
         if keyword_set(redgenum) then begin
            savefile = f[redgenum-1]
            redgenum = 0
         endif else begin
            num = intarr(nf)
            for j=0,nf-1 do num[j] = strmid( f[j], strpos(f[j],'edge')+4, 100 )
            f = f[sort(num)]
            num = num[sort(num)]
            for j=0,nf-1 do print, num[j], '    ', f[j]
            reply = ''
            while reply eq '' do begin
               print, 'Multiple saved edgefits detected.  Please select a number [1,' + $
                      strtrim(num[nf-1],2) + '] '
               read, reply
               reply = fix(reply)
               if reply lt 1 or reply gt num[nf-1] then reply=''
               reply = (where( num eq reply, count ))[0] + 1
               if count eq 0 then reply=''
            endwhile
            savefile = f[reply-1]
         endelse
      endelse
      print, 'Restoring edgefit information from '+savefile
      restore, savefile
   endelse
   if !d.name eq 'X' and not keyword_set(noplot) then begin
                                ;window, 14
      if keyword_set(linsam) then begin
                                ;plot_redge,redge_linsam,redge_sigma_linsam,/height,keywords=keywords
      endif else begin
         window, 14
         plot_redge,redge,redge_sigma,/height,keywords=keywords
      endelse 
   endif


; finding Daphnis coords
   sat = 635
   @get_sat_coords
   daphcoords = sat_polar[1]

; Plotting it as points
   ;plot_redge, redge,ps=3

; finding the wisp
   B = ''
   READ, B, PROMPT ="Are there wisps left on this profile? 'y' or 'n':" 
  
;exitting the program
   IF (B EQ 'q') THEN BEGIN
      retall
      ENDIF

   WHILE (B EQ 'y') DO BEGIN

      cursor, lon, rad, /data
      PRINT, lon, rad

; convert from redge to linsam
; redge_to_linsam()

; Indexing the wisp
      WispIndex = ''
      Read, WispIndex, PROMPT= "Wisp Number: "

; Storing the data
; if we already restored the save
      IF (keyword_set(wisplonrad)) THEN BEGIN
         wispim = [wispim, image_name]
; wisplinsam = [[wisplinsam], [line,sample]]
         wispdaph = [wispdaph,daphcoords]
         wisplonrad = [[wisplonrad],[lon,rad]]
         WispIndList = [WispIndList,WispIndex]

;if there is no save file
      ENDIF ELSE BEGIN
         wispim = [image_name]
         wispdaph = [daphcoords]
         wisplonrad = [lon,rad]
         WispIndList = [WispIndex]
; wisplinsam = [line, sample]
      ENDELSE

      save, wispim,wispdaph,wisplonrad,WispIndList,filename="wisps.sav"
      B = ''
      READ, B, PROMPT ="Are there wisps left on this profile? 'y' or 'n':"
      
 ; Exitting the program
      IF (B eq 'q') THEN BEGIN
         retall
         ENDIF

   ENDWHILE
   

  ; numimg=numimg +1
ENDFOR


END

