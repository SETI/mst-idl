function clip_edges, _im, xx

if not keyword_set(xx) then xx=2
im = _im
sz = size(im)
if sz[0] ne 2 then stop, 'Input image must have 2 dimensions.'
im[0:xx-1,*] = 0
im[*,0:xx-1] = 0
im[sz[1]-xx:sz[1]-1,*] = 0
im[*,sz[2]-xx:sz[2]-1] = 0

return, im

end
