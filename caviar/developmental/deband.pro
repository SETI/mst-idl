function deband, _im, xx, yy=yy

; Removes horizontal banding via a Fourier transform
if not keyword_set(xx) then xx = 1
if not keyword_exists(yy) then yy = 4
im = _im
sz = size(im)
ff = fft( im, -1 )
ff[ 0:xx-1, yy:sz[2]-yy-1 ] = 0
ff[ sz[1]-xx:sz[1]-1, yy:sz[2]-yy-1 ] = 0
;ff[ yy:sz[1]-yy-1, 0:xx-1 ] = 0
;ff[ yy:sz[1]-yy-1, sz[2]-xx:sz[2]-1 ] = 0

im2 = fft( ff, 1 )
im2 = abs(im2) * sign(real_part(im2))

return, im2

end
