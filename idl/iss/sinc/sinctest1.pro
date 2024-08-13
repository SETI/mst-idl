savefile = 'sinctest1.sav'
if keyword_set(findfile(savefile)) then restore, savefile else begin
  a = fltarr(200,200)+1
  nnk = 17
  nk = findgen(nnk)*5+10
  c = fltarr(3,nnk)
  for j=0,nnk-1 do begin
    print, nk[j]
    b = caviar_sincinterp3( a, nk[j], 2, /edge_wrap )
    c[*,j] = b[uniq( b, sort(b) )]
  endfor
  save, a, b, c, nk, filename=savefile
endelse

print, ''
print, 'c:'
print, c
print, ''
print, '        nk         0.5/nk       c[2]/c[1]    c[1]/c[0]'
print, [ reform(nk,1,nnk), 0.5/reform(nk,1,nnk), c[2,*]/c[1,*], c[1,*]/c[0,*] ]
print, ''

plot, nk, c[2,*], ps=-4, /xs, /ys, xtit='nk', ytit='c[2,*]'

end
