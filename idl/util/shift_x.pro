function shift_x, image, x

sz = size(image)
if sz[0] ne 2 then stop, 'Input image must be a 2-D array'
if x ge sz[1] then x = x mod sz[2]
if x eq 0 then return, image
if x lt 0 then begin
  return, [ image[-x:sz[1]-1,*], image[0:-x-1,*] ]
endif else begin
  return, [ image[sz[1]-x:sz[1]-1,*], image[0:sz[1]-x-1,*] ]
endelse

end
