radi = make_radi(mnrad,mxrad,mnlon,mxlon,rpj_sz,loni=loni)
!mouse.button = 1
xy = fltarr(2)
while !mouse.button ne 4 do begin
  cursor, x, y, 3, /device
  print, loni[x], radi[y]
  xy = [ [xy], [loni[x],radi[y]] ]
endwhile
xy = xy[*,1:n_elements(xy[0,*])-1]

end
