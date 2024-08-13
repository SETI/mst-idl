if not keyword_set(np) then np = n_elements(param)
bad = where( param[1:np-1] - param[0:np-2] lt -0.1  and $
             param[1:np-1] - param[0:np-2] gt -3*!dpi/2, nbad )
badi = 0
while nbad gt 0 do begin &$
  print, 'epicyclic_fix_angles: Iteration '+strtrim(badi+1,2) + ': ' + strtrim(nbad,2) &$
  ; One class: the element at bad is !pi higher than it should be
  ; A second class: the element at bad is halfway between a wrap
  ;        (picked up by the abs)
  badup = where( abs(param[bad]-param[bad-1]) gt !dpi/2 and $
                 abs(param[bad]-param[bad-1]) lt 3*!dpi/2, nbadup ) &$
  ; A third class: the element at bad+1 is !pi lower than it should be
  ; Forgo the abs so as not to correct the second class twice
  baddown = where( param[bad+2]-param[bad+1] gt !dpi/2 and $
                   param[bad+2]-param[bad+1] lt 3*!dpi/2, nbaddown ) &$
  if nbadup gt 0 then param[bad[badup]] = param[bad[badup]] - !dpi &$
  if nbaddown gt 0 then param[bad[baddown]+1] = param[bad[baddown]+1] + !dpi &$
  param = fix_angles( param, /rad, /to360 ) &$
  nbad_last = nbad &$
  bad = where( param[1:np-1] - param[0:np-2] lt -0.1  and $
               param[1:np-1] - param[0:np-2] gt -3*!dpi/2, nbad ) &$
  if nbad eq nbad_last then stop &$
  badi = badi + 1 &$
endwhile
