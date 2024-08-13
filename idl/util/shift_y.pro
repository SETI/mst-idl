function shift_y, image, y

sz = size(image)
if sz[0] ne 2 then stop, 'Input image must be a 2-D array'
if y ge sz[2] then y = y mod sz[2]
if y eq 0 then return, image
if y lt 0 then begin
  return, [ [image[*,-y:sz[2]-1]], [image[*,0:-y-1]] ]
endif else begin
  return, [ [image[*,sz[2]-y:sz[2]-1]], [image[*,0:sz[2]-y-1]] ]
endelse

end
