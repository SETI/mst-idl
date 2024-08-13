function gradient_mst, im, mag=mag

sz = size(im)
if sz[0] ne 2 then stop, 'Expecting a 2-D image'
gradx = im*0
grady = im*0

for j=0,sz[1]-1 do gradx[*,j] = deriv( im[*,j] )
for j=0,sz[2]-1 do grady[j,*] = deriv( im[j,*] )

if keyword_set(mag) then begin
  return, sqrt( gradx^2 + grady^2 )
endif else return, [ [[gradx]], [[grady]] ]

end
