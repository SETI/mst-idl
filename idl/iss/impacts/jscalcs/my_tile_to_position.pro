function my_tile_to_position,nx,ny,leftmargin=leftmargin,rightmargin=rightmargin,topmargin=topmargin,bottommargin=bottommargin,interxmargin=interxmargin,interymargin=interymargin


leftmar=.1
if keyword_set(leftmargin) then leftmar=leftmargin*leftmar
rightmar=.05
if keyword_set(rightmargin) then rightmar=rightmargin*rightmar
topmar=.05
if keyword_set(topmargin) then topmar=topmargin*topmar
bottommar=.1
if keyword_set(bottommargin) then bottommar=bottommargin*bottommar
interx=.1
if keyword_set(interxmargin) then interx=interxmargin*interx
intery=.05
if keyword_set(interymargin) then intery=interymargin*intery

dx=(1.-leftmar-rightmar-float(nx-1)*interx)/float(nx)
dy=(1.-topmar-bottommar-float(ny-1)*intery)/float(ny)

xl=fltarr(nx)
xl(0)=leftmar
for ix=1,nx-1 do begin
xl(ix)=xl(ix-1)+dx+interx
endfor

yd=fltarr(ny)
yd(0)=bottommar
for iy=1,ny-1 do begin
yd(iy)=yd(iy-1)+dy+intery
endfor

ind=sort(-yd)
yd=yd(ind)

pos=fltarr(nx*ny,4)

for ic=0,nx*ny-1 do begin
countx=(ic mod nx)
county=ic/nx
;print,countx,county
pos(ic,*)=[xl(countx),yd(county),xl(countx)+dx,yd(county)+dy]
endfor

return,pos
end
