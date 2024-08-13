function caviar_edgefind, _rawim, mnrad, mxrad, mnlon, mxlon, et, polera, poledec, cam_params, nl, cmat, vobs_planet, noplot=noplot, gaussian=gaussian, gaussfilt=gaussfilt, sincfilt=sincfilt, sincmm=sincmm, sincnn=sincnn, debug=debug

if not keyword_exists(noplot) then noplot = 1
nnn = 3600
; If not all inputs are set, then use bounding values for entire image
noradarray = keyword_set(mnrad) and keyword_set(mxrad) and $
             keyword_set(mnlon) and keyword_set(mxlon)
if not keyword_set(noradarray) then get_radarray, cam_params, cmat, nl, et, $
   polera, poledec, -82l, radarray, lonarray, outofplane=outofplane, $
   /minmaxonly, planet_coords=planet_coords
if not keyword_set(mnrad) then mnrad = min(radarray)
if not keyword_set(mxrad) then mxrad = max(radarray)
if not keyword_set(mnlon) then mnlon = min(lonarray)
if not keyword_set(mxlon) then mxlon = max(lonarray)
; Reverse min and max radius if necessary
if mnrad gt mxrad then begin
	_mnrad=mxrad
	_mxrad=mnrad
endif else begin
	_mxrad=mxrad
	_mnrad=mnrad
endelse
; Make sure longitudes are double-precision
mnlon = double(mnlon)
mxlon = double(mxlon)
; Get image coordinates corresponding to the selected region
get_ring, et, [_mnrad,_mxrad], mnlon, mxlon, polera, poledec, nnn, $
         region, 699L, light_time=light_time
image_coords, region, cmat, vobs_planet, cam_params, nl, $
              region_coords
; Second line in reverse order, so it makes a single polygon
for j=0,1 do region_coords[nnn:2*nnn-1,j] = $
   reverse( region_coords[nnn:2*nnn-1,j] )
; Plot polygon
plots, /device, round(region_coords[[lindgen(2*nnn),0],1]), $
       (nl-1)-round(region_coords[[lindgen(2*nnn),0],0]), $
       color=make_array( n_elements(region[*,0])+1, value=green() )
; Find pixels inside the region
sz = size( _rawim, /dimensions )
region_indices = polyfillv( region_coords[*,1], region_coords[*,0], $
                            sz[0], sz[1] )
region_mask = bytarr( sz[0], sz[1] )
region_mask[region_indices] = 1

; Begin code adapted from CANNY
if keyword_set(sincfilt) then begin
  if not keyword_set(sincmm) then sincmm = 15;30
  if not keyword_set(sincnn) then sincnn = 3
buie = 1
  if keyword_set(buie) then begin
    rawim = fltarr( sz[0]*sincnn, sz[1]*sincnn )
    xloc = findgen(sz[0]) #  replicate(1,sz[1])
    yloc = findgen(sz[1]) ## replicate(1,sz[0])
    j0 = -(sincnn-( sincnn mod 2 eq 1 ))/2
    j1 = j0 + sincnn - 1
    for j=j0,j1 do for k=j0,j1 do begin
      rawim[ sincnn*xloc+j0-j, sincnn*yloc+j0-k ] = $
                             sshift2d( _rawim, [j,k]/float(sincnn) )
    endfor 
    sz = size( rawim, /dimensions )
    ; Create arrays of x|y locations in output image
    xloc1 = lindgen(sz[0]) #  replicate( 1, sz[1] )
    yloc1 = lindgen(sz[1]) ## replicate( 1, sz[0] )
    ; For output image, create arrays of x|y locations in input image
    ; For example, nn=3 and sz[0]=100:  
    ; [ -1/3, 0, 1/3, 2/3, 1, 1 1/3, ... 98 1/3, 98 2/3, 99, 99 1/3 ]
    ; For example, nn=4 and sz[0]=100:  
    ; [ -1/2, -1/4, 0, 1/4, 1/2, 3/4, 1, 1 1/4, ... 98 1/2, 98 3/4, 99, 99 1/4 ]
    xloc2 = ( float(xloc1) - (sincnn-( sincnn mod 2 eq 1 ))/2 )/sincnn
    yloc2 = ( float(yloc1) - (sincnn-( sincnn mod 2 eq 1 ))/2 )/sincnn
  endif else begin
    rawim = caviar_sincinterp3( _rawim, sincmm, sincnn, $
                                xloc1=xloc1, yloc1=yloc1,$
                                xloc2=xloc2, yloc2=yloc2 ) ;, region_indices )
  endelse
  ; Update size (now scaled by sincnn) and quantities derived from it
  sz = size( rawim, /dimensions )
  region_mask = rebin( region_mask, sz[0], sz[1], /sample )
  foo = where( rawim eq 0, count )
  if count gt 0 then region_mask[foo] = 0
;  if not keyword_set( edge_wrap or edge_truncate ) then begin
    region_mask[0:sincmm*sincnn,*] = 0
    region_mask[*,0:sincmm*sincnn] = 0
    region_mask[sz[0]-sincmm*sincnn-1:sz[0]-1,*] = 0
    region_mask[*,sz[1]-sincmm*sincnn-1:sz[1]-1] = 0
;  endif
  region_indices = where( region_mask )
  ; Smooth over imperfections in finite sinc kernel
  __rawim = rawim
  rawim = smooth( rawim, sincnn )
endif else if keyword_set(gaussfilt) then begin
  ; Smooth with gaussian filter
  gksigma = 0.6
  gkwidth = 5 < min(sz)
  ;; create X and Y indices
  gkx = (dindgen(gkwidth)-gkwidth/2) # replicate(1,gkwidth)
  gky = transpose(gkx)
  ;; create kernel
  gaussKernel = exp( -((gkx^2)+(gky^2))/(2*double(gksigma)^2)) / (sqrt(2.0*!pi) * double(gksigma) )
  ;; give it an appropriate scale factor
  scale = 1/min(gaussKernel) < 16384 
  gaussKernel = TEMPORARY(gaussKernel)*scale + 1d-6 
  gaussKernel = long(gaussKernel)
  rawim = convol( _rawim, gaussKernel, total(gaussKernel) )
endif else rawim = _rawim
; Find gradient of image using Sobel convolution masks
print, 'Finding gradient...'
gxKern = [[-1, 0, 1],[-2, 0, 2],[-1, 0, 1]]
gyKern = [[-1,-2,-1],[ 0, 0, 0],[ 1, 2, 1]]
gx = convol( rawim, gxKern, total(gxKern) )
gy = convol( rawim, gyKern, total(gyKern) )
; Find edge direction and magnitude
mag = sqrt( gx^2 + gy^2 )
; Zero the 3-pixel edges left over from boundary conditions from
; the smoothing and the convolution
; Left
mag[0:2, 0:sz[1]-1] = 0
; Right
mag[sz[0]-3:sz[0]-1, 0:sz[1]-1] = 0
; Bottom
mag[0:sz[0]-1, 0:2] = 0
; Top
mag[0:sz[0]-1, sz[1]-3:sz[1]-1] = 0
; Calculate angles of gradient
theta = atan(gy, gx)
; Get sector of angle : 0-3 (see CANNY documentation)
; 0 is E/W, 1 is NE/SW, 2 is N/S, 3 is NW/SE
; Make array to hold result
sector = make_array( size=size(theta), type=1 )
; Group angles in bins of 22.5 degrees
hist = histogram( theta, min=-!pi, max=!pi, $
                  binsize=!dtor*45/2, reverse_indices=rev )
; Assign sector value to all angles
for i=0,15 do if (hist[i] ne 0) then begin 
  sector[rev[ rev[i] : rev[i+1]-1 ]] = (i+1)/2 mod 4
endif
; Create arrays of current x|y locations
xloc = lindgen(sz[0]) #  replicate(1, sz[1])
yloc = lindgen(sz[1]) ## replicate(1, sz[0])
; Get arrays of x|y offsets based on sector
; xoffset = ([1,1,0,-1])[sector]
; yoffset = ([0,1,1,1])[sector]
xoffset = ((tmp=(indgen(4)-2)*(-1))/(tmp>1))[sector]
yoffset = (indgen(4) GT 0)[sector]
; Special case where magnitude equals 0.  No magnitude means no offsets.
wh = where( ~mag, cnt )
if (cnt ne 0) then begin
  xoffset[wh] = 0
  yoffset[wh] = 0
endif
; Get points immediately up and down gradient
side1 = mag[ xloc+xoffset, yloc+yoffset ]
side2 = mag[ xloc-xoffset, yloc-yoffset ]
; Only accept points that are local maximum in direction of the gradient
nmsupp_mask = (mag gt side1) and (mag gt side2)
; Further, only accept points within the selected region
nmsupp_mask = nmsupp_mask * region_mask
; Get maxima magnitudes
suppMag = nmsupp_mask * mag
; End code adapted from CANNY

; Assuming that we are looking for an edge that is much sharper than 
; any other in the selected area, rank selected points by magnitude
; and look for a big gap
sortmags = suppMag[where( suppMag )]
sortmags = sortmags[sort(sortmags)]
dsortmags = sortmags[1:n_elements(sortmags)-1] - sortmags
; Largest gap
mxdsm1 = max(dsortmags)
; Make sure second-largest gap is much smaller than largest gap
mxdsm2 = max(dsortmags[where( dsortmags ne mxdsm1 )])
if mxdsm2 gt mxdsm1/5 then stop, 'More than one significant gap in edge mag'
; Now select edge magnitudes above the large gap
minmag = sortmags[(where( dsortmags eq mxdsm1 ))[0]+1]
nmsupp_mask = suppMag ge minmag
suppMag = nmsupp_mask * mag
; Also get sectors for selected points
; Since 0 in sector is meaningful, set non-selected points to -1
suppSec = nmsupp_mask * (sector+1) - 1

print, 'Finding edge locations...'
; Find selected points
foo = where( nmsupp_mask, count )
if count eq 0 then begin
  if keyword_set(debug) then stop, 'No points'
  return, -1
endif
; Create edge array to be filled, 2xn array with rows = [line,sample]
redge_linsam = fltarr( 2, count )
; Now fit a quadratic to the three points centered on each selected point,
; with the direction determined by the sector
for j=0,count-1 do begin
  if keyword_set(gaussian) then nxx = 11 else nxx = 3
  xx = findgen(nxx) - (nxx-1)/2
  ;yy = [ side2[foo[j]], mag[foo[j]], side1[foo[j]] ]
  locs = [ [ xloc[foo[j]] + xoffset[foo[j]]*xx ], [ yloc[foo[j]] + yoffset[foo[j]]*xx ] ]
  indices = locs[*,1]*sz[1] + locs[*,0]
  for k=nxx-1,0,-1 do if (where(indices[k] eq region_indices))[0] eq -1 then begin
    xx = vec_remove( xx, k )
  endif
  nxx = n_elements(xx)
  yy = mag[indices]
  if keyword_set(gaussian) then begin
    gg = gaussfit( xx, yy, pp, nterms=4, sigma=_redge_sigma )
  endif else begin
    ; Fit to a quadratic in hk-form
    ; Note that there are no error bars.  Three data points, three DOFs.
    weights = replicate( 1.0, n_elements(yy) )
    pp = [ mag[foo[j]], 0, 1 ]
    gg = mpcurvefit( xx, yy, weights, pp, function_name='quadratic_hk', /quiet )
  endelse
  if not keyword_set(noplot) then begin
    winset, 8, nowin
    if keyword_set(nowin) then window, 8
    plot, xx, yy, ps=4
    xx1 = findgen((max(xx)-min(xx))*50+1)/50 - (max(xx)-min(xx))/2
    if keyword_set(gaussian) then begin
      yy1 = gaussian( xx1, pp )
    endif else begin
      quadratic_hk, xx1, pp, yy1
    endelse
    oplot, xx1, yy1
  endif 
  if abs(pp[1]) gt 1 then begin
    print, 'Fitted peak outside interval: ' + strtrim(j,2) + ' ' + $
           strtrim(xloc[foo[j]],2) + ' ' + strtrim(yloc[foo[j]],2)
  endif else begin
    redge_linsam[*,j] = [ yloc[foo[j]] + pp[1]*yoffset[foo[j]], $
                          xloc[foo[j]] + pp[1]*xoffset[foo[j]] ]
  endelse
endfor

if keyword_set(sincfilt) then begin
  xloc1 = xloc1[*,0]
  xloc2 = xloc2[*,0]
  yloc1 = reform(yloc1[0,*])
  yloc2 = reform(yloc2[0,*])
  redge_linsam[0,*] = interpol( yloc2, yloc1, redge_linsam[0,*] )
  redge_linsam[1,*] = interpol( xloc2, xloc1, redge_linsam[1,*] )
endif

if keyword_set(debug) then stop, 'Stopped for debugging.'
return, redge_linsam

end
