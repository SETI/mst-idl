pro radial_scan_u, _rawim, radi, val, et, polera, poledec, pixscale, nl, $
       cmat, vobs_planet, mnrad=mnrad, mxrad=mxrad, mnlon=mnlon, mxlon=mxlon,$
       resfac=resfac, zoomfactor=zoomfactor, noscan=noscan, phi=phi, $
       nocrop=nocrop, errbar=errbar, radscan_np=radscan_np, crays=_cray, $
       cray_space=cray_space, color=color, vertical=vertical, ansalon=ansalon, $
       radarray=radarray, lonarray=lonarray, outofplane=outofplane, $
       primary=primary

; This version of radial scan uses the polyflux algorithm created by Bob Hogan
; as part of Casvu.  It is slower but more accurate than my own radial_scan.pro

rawim = _rawim
;nocrop = 1
mask = bytarr(nl,nl) + 1
if not keyword_set(nocrop) then begin
  ; Image edges are often dodgy.
;  rawim = rawim[2:nl-3,2:nl-3]
  offset = 0;2
;mask[0:1,*] = 0             ;Rob masking out the crap distorted blank edges due to geometric correction
;mask[*,0:1] = 0
;mask[nl-2:nl-1,*] = 0
;mask[*,nl-2:nl-1] = 0
mask[0:100,*] = 0
mask[*,0:100] = 0
mask[nl-100:nl-1,*] = 0
mask[*,nl-100:nl-1] = 0
endif else offset = 0
_nl = nl - offset*2
if not keyword_set(resfac) then resfac = 1.5   ; Governs bin spacing
if not keyword_set(zoomfactor) then zoomfactor = 10  ; Governs pixel division

if not (keyword_set(mnrad) and keyword_set(mxrad) and keyword_set(mnlon) and keyword_set(mxlon)) then begin
  if keyword_set(outofplane) then stop
endif
if not keyword_set(mnrad) then mnrad = min(radarray)
if not keyword_set(mxrad) then mxrad = max(radarray)
if not keyword_set(mnlon) then mnlon = min(lonarray)
if not keyword_set(mxlon) then mxlon = max(lonarray)

; Set the active window to #1 (the main Caviar display), but only if it exists.
winset, 1, no_w1

state = 0
nnn = 360
have_already_rerun = 0
phi_rerun_polygon:
mnrad = (double(mnrad))[0]
mxrad = (double(mxrad))[0]
mnlon = (double(mnlon))[0]
mxlon = (double(mxlon))[0]
if not keyword_set(primary) then primary=799L
if keyword_set(vertical) then begin
  get_rz,et,mnrad,mnlon,mxlon,polera,poledec,nnn,radscan_region1,primary,state=state,ansalon=ansalon
  get_rz,et,mxrad,mnlon,mxlon,polera,poledec,nnn,radscan_region2,primary,state=state,ansalon=ansalon
endif else begin
  get_ring,et,mnrad,mnlon,mxlon,polera,poledec,nnn,radscan_region1,primary,-32L,state=state
  get_ring,et,mxrad,mnlon,mxlon,polera,poledec,nnn,radscan_region2,primary,-32L,state=state
endelse
image_coords_u,radscan_region1,cmat,vobs_planet,pixscale,nl,radscan_region1_coords
image_coords_u,radscan_region2,cmat,vobs_planet,pixscale,nl,radscan_region2_coords
radscan_region_coords = [ radscan_region1_coords, reverse(radscan_region2_coords), radscan_region1_coords[0,*] ]
if not keyword_set(color) then color=green()
if not keyword_set(no_w1) then plots, round(radscan_region_coords[*,1]), (nl-1)-round(radscan_region_coords[*,0]), linestyle=0, color=color, /device, thick=2

if keyword_set(phi) then begin

  if not keyword_set(have_already_rerun) then begin
    ; Path length along inner/outer edge
    nbins = long( resfac * mean([ path_length(radscan_region1_coords), $
                                  path_length(radscan_region2_coords) ]) )
    nnn = long(nbins) + 1
    have_already_rerun = 1
    goto, phi_rerun_polygon
  endif  
  radbins = findgen(nbins+1) / (nbins) * (mxlon-mnlon) + mnlon
  inner_coords = radscan_region_coords[nnn*2-1:nnn*2,*]

endif else begin

  ; Mean length, in pixels, from inner to outer edge
  nbins = long( resfac * mean(sqrt( $
     (radscan_region1_coords[*,0]-radscan_region2_coords[*,0])^2 + $
     (radscan_region1_coords[*,1]-radscan_region2_coords[*,1])^2 )) )
  radbins = findgen(nbins+1) / (nbins) * (mxrad-mnrad) + mnrad
  inner_coords = radscan_region1_coords

endelse
radi = ( radbins[1:nbins] + radbins[0:nbins-1] ) / 2
val = fltarr(nbins)
radscan_np = fltarr(nbins)
errbar = fltarr(nbins)
if keyword_set(_cray) then begin
  p2radec_quicker_u,pixscale,cmat,nl,_cray[*,0],_cray[*,1],cray_RA,cray_dec
  p2ralon_quicker_u,cmat,et,polera,poledec,-82L,cray_RA,cray_dec,crayr,crayl
  if not keyword_exists(cray_space) then cray_space = 4
  for j=-cray_space,cray_space do for k=-cray_space,cray_space do begin
    mask[(_cray[*,1]+j)>0<(nl-1),(_cray[*,0]+k)>0<(nl-1)] = 0
  endfor
endif

if not keyword_set(noscan) then for i=0,nbins-1 do begin
  if i mod fix(float(nbins)/20) eq 0 then begin
    if not keyword_set(no_w1) then begin
      plots, round(inner_coords[*,1]), (nl-1)-round(inner_coords[*,0]), $
             linestyle=0, color=color, /device 
    endif else print, i, ' / ', nbins
  endif
  if keyword_set(phi) then begin
    polygon = radscan_region_coords[ [i,i+1,nnn*2-2-i,nnn*2-1-i], * ]
    inner_coords = polygon[ [0,3], * ]
  endif else begin
    if keyword_set(vertical) then begin
      get_rz,et,radbins[i+1],mnlon,mxlon,polera,poledec,nnn,outer,primary,state=state,ansalon=ansalon
    endif else begin
      get_ring,et,radbins[i+1],mnlon,mxlon,polera,poledec,nnn,outer,primary,-32L,state=state
    endelse
    image_coords_u,outer,cmat,vobs_planet,pixscale,nl,outer_coords
    polygon = [ inner_coords, reverse(outer_coords) ] - offset
    inner_coords = outer_coords
  endelse
  ; get_ring gives [x,y] coords in rows [1,0].  
  ; polyflux wants them in rows [0,1]
  polygon = rotate( polygon, 7 )
  val[i] = polyflux( polygon/_nl, rawim, _nl, _nl, zoomfactor, errbar=_errbar, radscan_np=_np, mask=mask )
  radscan_np[i] = _np
  errbar[i] = _errbar
endfor

foo = where( val eq -1, count )
if count gt 0 then begin
  val = vec_remove( val, foo )
  radi = vec_remove( radi, foo )
  radscan_np = vec_remove( radscan_np, foo )
  errbar = vec_remove( errbar, foo )
endif

end
