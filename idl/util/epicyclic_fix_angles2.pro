if not keyword_set(np) then np = n_elements(param)
bad = where( abs(param-median(param,10)) gt !dpi/2 and $
             abs(param-median(param,10)) lt 3*!dpi/2, nbad )
badi = 0
while nbad gt 0 do begin &$
  print, 'epicyclic_fix_angles2: Iteration '+strtrim(badi+1,2) + ': ' + strtrim(nbad,2) &$
  param[bad] = param[bad] + !dpi &$
  param = fix_angles( param, /rad, /to360 ) &$
  nbad_last = nbad &$
  bad = where( abs(param-median(param,10)) gt !dpi/2 and $
               abs(param-median(param,10)) lt 3*!dpi/2, nbad ) &$
  if nbad eq nbad_last then stop &$
  badi = badi + 1 &$
endwhile
