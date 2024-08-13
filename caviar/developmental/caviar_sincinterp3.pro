function caviar_sincinterp3, im, mm, nn, indices, debug=debug, xloc1=xloc1, yloc1=yloc1, xloc2=xloc2, yloc2=yloc2, edge_wrap=edge_wrap, edge_truncate=edge_truncate
  
; Return an version of the input image that is sub-sampled by a factor of nn
; using sinc interpolation. 

; im is the input image
; mm is how many points away the sinc should be calculated
; nn is the factor by which to increase the resolution
; indices (optional) is a subset of locations in the image to calculate

sz = size( im, /dimensions )
sz1 = sz*nn
im1 = fltarr( sz1[0], sz1[1] )
; Create arrays of x|y locations in input image
xloc = lindgen(sz[0]) #  replicate( 1, sz[1] )
yloc = lindgen(sz[1]) ## replicate( 1, sz[0] )
; Create arrays of x|y locations in output image
xloc1 = lindgen(sz1[0]) #  replicate( 1, sz1[1] )
yloc1 = lindgen(sz1[1]) ## replicate( 1, sz1[0] )
; For output image, create arrays of x|y locations in input image
; For example, nn=3 and sz[0]=100:  
; [ -1/3, 0, 1/3, 2/3, 1, 1 1/3, ... 98 1/3, 98 2/3, 99, 99 1/3 ]
; For example, nn=4 and sz[0]=100:  
; [ -1/2, -1/4, 0, 1/4, 1/2, 3/4, 1, 1 1/4, ... 98 1/2, 98 3/4, 99, 99 1/4 ]
xloc2 = ( float(xloc1) - (nn-( nn mod 2 eq 1 ))/2 )/nn
yloc2 = ( float(yloc1) - (nn-( nn mod 2 eq 1 ))/2 )/nn
; Array of integer locations (i.e., pixel locations in input image)
isint = ( xloc2 mod 1 eq 0 ) and ( yloc2 mod 1 eq 0 )

; Pixels to use
if not keyword_set(indices) then indices = where( im eq im )
mask = bytarr( sz[0], sz[1] )
mask[indices] = 1
mask1 = rebin( mask, sz1[0], sz1[1] )
indices1 = where( mask1 eq 1, count1 )

; For integer locations that are used, load in input values
im2 = fltarr( sz1[0], sz1[1] )
foo = where( mask1 and isint, count )
if count eq 0 then return, -1
im2[foo] = im[ xloc2[foo], yloc2[foo] ]
lims = [ min(xloc1[foo]), max(xloc1[foo]), min(yloc1[foo]), max(yloc1[foo]) ]

; Define sinc kernel
kernelsz = mm*nn
kernel = fltarr( 2*kernelsz+1 )/nn - mm
kernel = sinc(kernel) # sinc(kernel)

; For input into CONVOL, no sense crunching more zeroes than necessary
; Feed in only parts of im2 that are non-zero.  
; Unfortunately, can only specify this with rectangular coordinates. 
; Also, add margins of the size of the kernel
print, 'Starting convolution...'
imin = im2[ (lims[0]-kernelsz)>0:(lims[1]+kernelsz)<(sz1[0]-1), $
            (lims[2]-kernelsz)>0:(lims[3]+kernelsz)<(sz1[1]-1) ]
imout = convol( imin, kernel, edge_wrap=edge_wrap, edge_truncate=edge_truncate, total(kernel) )
im1[ (lims[0]-kernelsz)>0:(lims[1]+kernelsz)<(sz1[0]-1), $
     (lims[2]-kernelsz)>0:(lims[3]+kernelsz)<(sz1[1]-1) ] = imout

if keyword_set(debug) then stop

return, im1

end
