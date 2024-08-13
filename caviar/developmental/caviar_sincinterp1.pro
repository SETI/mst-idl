function caviar_sincinterp1, im, mm, nn, indices
  
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

for j=0l,count1-1 do begin
  ; Counter
  if j mod 100 eq 0 then print, strtrim(j,2) + ' / ' + strtrim(count1,2)
  ; Where we are in the output image
  xx1 = xloc1[indices1[j]]
  yy1 = yloc1[indices1[j]]
  ; Where we are in the input image
  xx2 = xloc2[indices1[j]]
  yy2 = yloc2[indices1[j]]
  ; Find integer points within mm input pixels
  subs = where( xloc2 ge xx2-mm and xloc2 le xx2+mm and $
                yloc2 ge yy2-mm and yloc2 le yy2+mm and isint eq 1 )
  dist2 = ( xloc2[subs] - xx2 )^2 + ( yloc2[subs] - yy2 )^2
  dist = sqrt(dist2[where( dist2 le mm^2+0.01 )])
  subs = subs[where( dist2 le mm^2+0.01, count2 )]
  mags = im[ xloc2[subs], yloc2[subs] ]  
  ; Do sinc interpolation
  sinc = sin(!pi*dist)/!pi/dist
  foo = (where( dist eq 0 ))[0]
  if foo ne -1 then sinc[foo] = 1
  im1[indices1[j]] = total(mags*sinc)
endfor

return, im1

end
