pro mkexed,v,exed,z

; For a vector of integers, v, sorts the elements of v into intervals.
; For example, v contains all integers between exed[0,i] and exed[1,i].
; z is the number of intervals (the y-dimension of exed), minus one.
; Written by Antonin Bouchez, 10/95.  Generalized by Matt Tiscareno, 12/00.

  if n_params() eq 0 then begin
    print,'SYNTAX:  MKEXED,v,exed,z'
    retall
  endif

  vv=v[sort(v)]
  vv=vv[uniq(vv)]

  zz=100
  redo:
  z=0
  exed=lonarr(2,zz)

  exed[0,z]=vv[0]

  for n=1l,n_elements(vv)-1 do begin
    if vv[n]-vv[n-1] gt 1 then begin
      exed[1,z]=vv[n-1]
      z=z+1
      if z eq zz then begin
        zz = zz*2
        goto, redo
      endif
      exed[0,z]=vv[n]
    endif
  endfor

  exed[1,z]=vv[n_elements(vv)-1]
  exed=exed[*,0:z]

return
end
