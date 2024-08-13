sfile = [ 'propellers.sav', 'prop_reproj.sav', 'prop_reproj_res.sav', 'fit_propellers4.sav' ]
nsfile = n_elements(sfile)
for j=0,nsfile-1 do if keyword_set(findfile(sfile[j]+'.old')) then stop, sfile[j]+'.old exists'
for j=0,nsfile-1 do if keyword_set(findfile(sfile[j])) then spawn, 'mv '+sfile[j]+' '+sfile[j]+'.old'

.run prop_anal
.run prop_reproj
.run prop_reproj_res

.res
restore, 'prop_reproj.sav.old'
images_old = images
prop_reproj_old = prop_reproj
restore, 'prop_reproj.sav'
npr = n_elements(prop_reproj)
npro = n_elements(prop_reproj_old)
oldinnew = intarr(npro)
for j=0,npro-1 do begin &$
  match = -1 &$
  for k=0,npr-1 do begin &$
    foo = where( prop_reproj_old[j].radlon eq prop_reproj[k].radlon, count ) &$
    if count eq 6 then begin &$
      if match eq -1 then match=k else match=[match,k] &$
    endif &$
  endfor &$
  if n_elements(match) eq 1 then oldinnew[j] = match else stop &$
endfor
print, oldinnew
save, oldinnew, npr, filename='oldinnew.sav'

.res
restore, 'oldinnew.sav'
restore, 'fit_propellers4.sav.old'
vars = [ '_chisq', '_dof', '_flag', '_initoffset', '_orphan', '_subtractavg', '_use', '_x1', '_x2', '_y1', '_y2' ]
type = [ 'flt', 'lon', 'str', 'int', 'int', 'int', 'int', 'lon', 'lon', 'lon', 'lon' ]
vars2 = [ '_pp', '_pp_sigma' ]
for j=0,n_elements(vars)-1 do begin &$
  ; temp = _chisq
  foo = execute( 'temp = '+vars[j] ) &$
  ; _chisq = fltarr(npr)
  foo = execute( vars[j]+' = '+type[j]+'arr(npr)' ) &$
  ; _chisq[oldinnew] = temp
  foo = execute( vars[j]+'[oldinnew] = temp' ) &$
endfor
for j=0,n_elements(vars2)-1 do begin &$
  ; temp = _pp
  foo = execute( 'temp = '+vars2[j] ) &$
  ; _pp = fltarr(8,npr)
  foo = execute( vars2[j]+' = fltarr(8,npr)' ) &$
  ; _pp[*,oldinnew] = temp
  foo = execute( vars2[j]+'[*,oldinnew] = temp' ) &$
endfor
save, _chisq, _dof, _flag, _initoffset, _orphan, _pp, _pp_sigma, _subtractavg, _use, _x1, _x2, _y1, _y2, $
  filename='fit_propellers4.sav'
