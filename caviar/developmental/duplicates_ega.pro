;Determining Duplicates after running AutoWisp_ega.pro

restore, "wisps.sav"
wisplist=[-1]
wisperr=[-1]
FOR ii=0, n_elements(WispIndList)-1 DO BEGIN
   addlist=[-1]
   
   IF (WispIndList[ii] EQ -1) THEN BEGIN
   ENDIF ELSE BEGIN
      finder=WispIndList[ii]
      FOR kk=0, n_elements(WispIndList)-1 DO BEGIN
         IF (finder EQ -1) THEN BEGIN
         ENDIF ELSE IF (finder EQ WispIndList[kk]) THEN BEGIN
            WispIndList[kk] = -1
            addlist=[addlist,wispdaph[kk]-wisplonrad[2*kk]]
        ENDIF
      ENDFOR
      sum = 0
      FOR zz=1, n_elements(addlist)-1 DO BEGIN
         sum = ABS(addlist[zz]) + sum
      ENDFOR
      avgwisploc = sum/(n_elements(addlist)-1)
      
      stdev=0
      FOR zz=1, n_elements(addlist)-1 DO BEGIN
         stdev=SQRT((ABS(addlist[zz])-ABS(avgwisploc))* (ABS(addlist[zz])-ABS(avgwisploc)) /(n_elements(addlist)-1))
      ENDFOR
      wisplist = [wisplist,avgwisploc]
      wisperr=[wisperr,stdev]
   ENDELSE
ENDFOR

wisplist2 = wisplist[1]
FOR ll=2, n_elements(wisplist)-1 DO BEGIN
   wisplist2=[wisplist2,wisplist[ll]]
ENDFOR

wisplist=wisplist2

wisperr2 = wisperr[1]
FOR ll=2, n_elements(wisperr)-1 DO BEGIN
   wisperr2=[wisperr2,wisperr[ll]]
ENDFOR

wisperr=wisperr2
      
save, wisplist,wisperr, filename='wisplist.sav'


END
