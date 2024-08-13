if not keyword_set(np) then np = n_elements(param)
bad = where( abs(param[1:np-1]-param[0:np-2]) gt !dpi/2 and $
             abs(param[1:np-1]-param[0:np-2]) lt 3*!dpi/2, nbad )
even = lindgen(nbad/2)*2
odd = even + 1
;if nbad mod 2 eq 1 then begin &$
;  even = [ even, nbad-1 ] &$
;  int1 = bad[even] - [-1,bad[odd]] &$
;  int2 = [bad[odd],np-1] - bad[even] &$
;endif else begin &$
;  int1 = [bad[even],np-1] - [-1,bad[odd]] &$
;  int2 = bad[odd] - bad[even] &$
;endelse
if nbad mod 2 eq 1 then begin &$
  even = [ even, nbad-1 ] &$
  exed1 = [ [0,bad[odd]+1], [bad[even]] ] &$
  exed2 = [ [bad[even]+1], [bad[odd],np-1] ] &$
endif else begin &$
  exed1 = [ [0,bad[odd]+1], [bad[even],np-1] ] &$
  exed2 = [ [bad[even]+1], [bad[odd]] ] &$
endelse
int1 = exed1[*,1] - exed1[*,0] + 1
int2 = exed2[*,1] - exed2[*,0] + 1
tot1 = total(int1)
tot2 = total(int2)
if tot1+tot2 ne np then print, 'WARNING: tot1+tot2 ne np'
if tot1 le tot2 then exed=exed1 else exed=exed2
foo = lonarr(tot1<tot2)
k = 0l
for j=0l,n_elements(exed[*,0])-1 do begin &$
  foo[k:k+exed[j,1]-exed[j,0]] = lindgen(exed[j,1]-exed[j,0]+1)+exed[j,0] &$
  k = k + exed[j,1] - exed[j,0] + 1 &$
endfor
param[foo] = param[foo] + !dpi
param = fix_angles( param, /rad, /to360 )
