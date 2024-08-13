print, 'You should have already opened in Caviar an image showing the propeller, and you should have already used @kidz to indicate the radial region to scan.'
if not keyword_exists(test) then test = 1
if keyword_set(test) then mnrad = 134890.0d0
if keyword_set(test) then mxrad = 134916.0d0
_mnrad = mnrad[0]
_mxrad = mxrad[0]
if not keyword_set(scanwidthpx) then scanwidthpx = 10.0d0
print, 'Taking scans of width ' + strtrim(scanwidthpx,2) + $
       ' times the longitudinal pixel scale.'
lonpx = (keywords.ringplane_aimpoint_longitudinal_scale_deg)[0]
scanwidth = scanwidthpx * lonpx
lolon = keywords.ringplane_least_orbital_longitude
golon = keywords.ringplane_greatest_orbital_longitude
if golon lt lolon then golon = golon + 360
nscans = round((golon-lolon)/scanwidth)
radscan_coords = dblarr(4,nscans)
nradi = 0
for j=0,nscans-1 do begin
  if j mod 10 eq 0 then print, 'j = '+strtrim(j,2)+' / '+strtrim(nscans,2)
  mnrad = _mnrad
  mxrad = _mxrad
  mnlon = lolon + scanwidth*j
  mxlon = lolon + scanwidth*(j+1)
  radscan_coords[*,j] = [ mnrad, mxrad, mnlon, mxlon ]
  radial_scan, rawim, radi, val, et, polera, poledec, cam_params, nl, cmat, $
               vobs_planet, $
               mnrad=mnrad, mxrad=mxrad, mnlon=mnlon, mxlon=mxlon,$
               resfac=resfac, zoomfactor=zoomfactor, noscan=noscan, phi=phi, $
                 nocrop=nocrop, radarray=radarray, lonarray=lonarray, $
               outofplane=outofplane, errbar=errbar, radscan_np=radscan_np, $
               crays=cray, cray_space=cray_space, /noclip, nbins=nradi, /quiet
  if j eq 0 then begin
    nradi = n_elements(radi)
    _radi = dblarr(nradi,nscans)
    _val = dblarr(nradi,nscans)
  endif 
  _radi[*,j] = radi
  _val[*,j] = val
endfor

end
