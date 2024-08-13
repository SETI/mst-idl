function square_elems,_elements,sx,sy
  
; For one-dimensional subscripts of a 2-D array with dimensions sx and sy,
; return 2-D subscripts.  Adapted from wher.com by Matt Tiscareno, 5/12/08.

dim=2
axes=[sx,sy]

elements = _elements
out=lonarr(dim,n_elements(elements))
num=lonarr(dim)+1
for i=1,dim-1 do num[i:dim-1]=num[i:dim-1]*axes[i-1]
for i=dim-1,0,-1 do begin
  rem=elements mod num[i]
  out[i,*]=(elements-rem)/num[i]
  elements=rem
endfor
  
return,out

end
