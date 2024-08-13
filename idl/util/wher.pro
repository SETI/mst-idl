function wher,expr,count

; Does the same thing as the tres-useful Where function, except it deciphers
; the element number into useful coordinates.  By Matt Tiscareno, 8/20/97.

szz=size(expr)

if szz[0] le 1 then return,where(expr) else begin
  dim=szz[0]
  axes=szz[1:dim]

  elements=where(expr,count)
  if count eq 0 then return,-1 else begin
    out=lonarr(dim,count)
    num=lonarr(dim)+1
    for i=1,dim-1 do num[i:dim-1]=num[i:dim-1]*axes[i-1]
    for i=dim-1,0,-1 do begin
      rem=elements mod num[i]
      out[i,*]=(elements-rem)/num[i]
      elements=rem
    endfor
  endelse
  
  return,out

endelse
 
end