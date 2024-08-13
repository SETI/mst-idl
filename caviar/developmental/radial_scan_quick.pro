pro radial_scan_quick, rawim, radarray, lonarray, val, radi, phi=phi, $
	mnlon=_mnlon, mxlon=_mxlon, mnrad=_mnrad, mxrad=_mxrad, $
	resfac=_resfac, noscan=noscan, nocrop=nocrop, radbins=_radbins

; This is the old radial scan method, formerly radial_scan.pro.  It runs in 
; only 40% of the time of the new method (which is now radial_scan.pro).  
; However, it is less accurate, and has a systematic error in the
; radial variable (traceable to get_radarray, I think).

if keyword_set(phi) then begin
  temp = radarray
  radarray = lonarray
  lonarray = temp
  if keyword_set(_mnrad) then mnlon = _mnrad
  if keyword_set(_mxrad) then mxlon = _mxrad
  if keyword_set(_mnlon) then mnrad = _mnlon
  if keyword_set(_mxlon) then mxrad = _mxlon
endif else begin
  if keyword_set(_mnrad) then mnrad = _mnrad
  if keyword_set(_mxrad) then mxrad = _mxrad
  if keyword_set(_mnlon) then mnlon = _mnlon
  if keyword_set(_mxlon) then mxlon = _mxlon
endelse

szim = size(rawim)
area = intarr( szim[1], szim[2] ) + 1
if not keyword_set(nocrop) then begin
  ; Image edges are often dodgy.
  area[0:1,*] = 0
  area[szim[1]-2:szim[1]-1,*] = 0
  area[*,0:1] = 0
  area[*,szim[2]-2:szim[2]-1] = 0
endif
if keyword_set(mnlon) then begin
  foo = where( lonarray lt mnlon, count )
  if count gt 0 then area[foo] = 0
endif
if keyword_set(mxlon) then begin
  foo = where( lonarray gt mxlon, count )
  if count gt 0 then area[foo] = 0
endif

if keyword_set(mnrad) then begin
  mnrad = min( radarray[where( area eq 1 and radarray gt mnrad )] )
endif else mnrad = min( radarray[where(area)] )
if keyword_set(mxrad) then begin
  mxrad = max( radarray[where( area eq 1 and radarray lt mxrad )] )
endif else mxrad = max( radarray[where(area)] )
mnradloc = ( wher( radarray eq mnrad and area eq 1 ) )[*,0]
mxradloc = ( wher( radarray eq mxrad and area eq 1 ) )[*,0]
nbins = sqrt( (mnradloc[0]-mxradloc[0])^2 + (mnradloc[1]-mxradloc[1])^2 )
if keyword_set(_resfac) then resfac=_resfac else resfac = 1
nbins = fix( nbins*resfac )

if keyword_set(_radbins) then begin
  radbins = _radbins
  nbins = n_elements(radbins) - 1
endif else radbins = findgen(nbins+1) / (nbins) * (mxrad-mnrad) + mnrad
radi = ( radbins[1:nbins] + radbins[0:nbins-1] ) / 2
val = fltarr(nbins)
if not keyword_set(noscan) then for j=0,nbins-1 do begin
  if j mod fix(nbins/20) eq 0 then print, j+1, ' / ', fix(nbins)
  use = where( radarray ge radbins[j] and radarray le radbins[j+1] and area eq 1, count )
  if count eq 0 then val[j] = -1 else val[j] = mean( rawim[use] )
endfor
foo = where( val eq -1, count )
if count gt 0 then begin
  val = vec_remove( val, foo )
  radi = vec_remove( radi, foo )
endif

end
