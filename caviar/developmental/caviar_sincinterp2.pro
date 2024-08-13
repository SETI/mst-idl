function caviar_sincinterp2, im, mm, nn, indices, debug=debug
  
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

; Set mm to a value where the integrated kernel is zero
; To get the volume under the curve, integrate 2*!pi*r * sin(!pi*r) / (!pi*r), 
; which is simply equal to 2*sin(!pi*r).  
; The integral of this is -(2/!pi)*cos(!pi*r)
; Thus the integral goes to zero when r is an integer plus a half
mm = round( mm + 0.5 ) - 0.5

; Get relative locations in output image within mm*nn pixels of any point
circlex = cos(findgen(361)*!pi/180) 
circley = sin(findgen(361)*!pi/180) 
elements = polyfillv( sz1[0]/2+(mm*nn-.01)*circlex, $
                      sz1[1]/2+(mm*nn-.01)*circley, sz1[0], sz1[1] )
nelem = n_elements(elements)
elements = square_elems( elements, sz1[0], sz1[1] )
elements = [ elements[0,*] - sz1[0]/2 + 1, elements[1,*] - sz1[1]/2 + 1 ]
; Check symmetry
if (where( -reverse(elements,2) ne elements ))[0] ne -1 then stop
;; Remove point where displacement is zero (will be done manually later)
;zp = (where( elements[0,*] eq 0 and elements[1,*] eq 0, nzp ))[0]
;if nzp ne 1 then stop
;elements = elements[*,vec_remove( lindgen(nelem), zp )]
;nelem = nelem - nzp
; Get relative distance in input image for each location, then evaluate sinc
dist = reform(sqrt( elements[0,*]^2 + elements[1,*]^2 )) / nn
sinc = sin(!pi*dist) / !pi / dist
sinc[where(dist eq 0)] = 1

kernelsz = max(elements) > min(elements)
kernel = fltarr( 2*kernelsz+1, 2*kernelsz+1 )
kernel[ elements[0,*]-min(elements), elements[1,*]-min(elements) ] = sinc

; For input into CONVOL, no sense crunching more zeroes than necessary
; Feed in only parts of im2 that are non-zero.  
; Unfortunately, can only specify this with rectangular coordinates. 
; Also, add margins of the size of the kernel
print, 'Starting convolution...'
imin = im2[ (lims[0]-kernelsz)>0:(lims[1]+kernelsz)<(sz1[0]-1), $
            (lims[2]-kernelsz)>0:(lims[3]+kernelsz)<(sz1[1]-1) ]
imout = convol( imin, kernel )
im1[ (lims[0]-kernelsz)>0:(lims[1]+kernelsz)<(sz1[0]-1), $
     (lims[2]-kernelsz)>0:(lims[3]+kernelsz)<(sz1[1]-1) ] = imout

if keyword_set(debug) then stop

return, im1

end
