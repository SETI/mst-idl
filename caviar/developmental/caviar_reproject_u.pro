pro caviar_reproject_u,im,rawim,mnrad,mxrad,mnlon,mxlon,et,polera,$
poledec,pixscale,nl,cmat,vobs_planet,ry,rx,rzoom,resfac,forcexy=forcexy,$
reprojected_image=rpi, raw_reprojected_image=rrpi, zoom_reprojected_image=zrpi,$
interp=interp, planet_coords=planet_coords, light_time=light_time,$
noplot=noplot, silent=silent, reproj_xygrid=reproj_xygrid, cubic=cubic, sinc=sinc

wset=1
nnn = 3600
; If not all inputs are set, then use bounding values for entire image
noradarray = keyword_set(mnrad) and keyword_set(mxrad) and $
             keyword_set(mnlon) and keyword_set(mxlon)
if not keyword_set(noradarray) then get_radarray_u, pixscale, cmat, nl, et, $
   polera, poledec, -32l, radarray, lonarray, outofplane=outofplane, $
   /minmaxonly, planet_coords=planet_coords
if not keyword_set(mnrad) then mnrad = min(radarray)
if not keyword_set(mxrad) then mxrad = max(radarray)
if not keyword_set(mnlon) then mnlon = min(lonarray)
if not keyword_set(mxlon) then mxlon = max(lonarray)
; Reverse min and max radius if necessary
if mnrad gt mxrad then begin
	rpj_mnrad=mxrad
	rpj_mxrad=mnrad
endif else begin
	rpj_mxrad=mxrad
	rpj_mnrad=mnrad
endelse
; Make sure longitudes are double-precision
mnlon = double(mnlon)
mxlon = double(mxlon)
; Get image coordinates corresponding to the selected region
get_ring,et,[rpj_mnrad,rpj_mxrad],mnlon,mxlon,polera,poledec,nnn,reproj_region,799L,-32L, light_time=light_time
image_coords_u,reproj_region,cmat,vobs_planet,pixscale,nl,reproj_region_coords
; Second line in reverse order, so it makes a single polygon
for j=0,1 do reproj_region_coords[nnn:2*nnn-1,j] = $
   reverse( reproj_region_coords[nnn:2*nnn-1,j] )
; Plot polygon
if not keyword_set(noplot) then begin
  plots, /device, round(reproj_region_coords[[lindgen(2*nnn),0],1]), $
         (nl-1)-round(reproj_region_coords[[lindgen(2*nnn),0],0]), $
         color=make_array( n_elements(reproj_region[*,0])+1, value=green() )
endif

if keyword_set(forcexy) and keyword_set(rx) and keyword_set(ry) then begin
endif else begin
  forcexy = 0
  if not keyword_set(resfac) then resfac=2
  ry = resfac * mean(sqrt( $
     ( reproj_region_coords[0:nnn-1,0] - $
       reverse(reproj_region_coords[nnn:2*nnn-1,0]) )^2 + $
     ( reproj_region_coords[0:nnn-1,1] - $
       reverse(reproj_region_coords[nnn:2*nnn-1,1]) )^2 ))
  rx = resfac * mean([ path_length(reproj_region_coords[nnn:2*nnn-1,*]), $
                       path_length(reproj_region_coords[0:nnn-1,*]) ])
endelse

if not keyword_set(rx) then rx=1000 else rx=round(rx)
if not keyword_set(ry) then ry=100 else ry=round(ry)
if not keyword_set(rzoom) then rzoom=1

if not keyword_set(silent) then begin
  print, 'Reprojection Parameters:'
  print, 'mnrad = '+strtrim(rpj_mnrad,2)+', mxrad = '+strtrim(rpj_mxrad,2)+', mnlon = '+strtrim(mnlon,2)+', mxlon = '+strtrim(mxlon,2)
  if keyword_set(forcexy) then begin
    aa = ' (directly specified)'
  endif else begin
    print, 'Resolution factor = '+strtrim(resfac,2)
    aa = ''
  endelse
  print, 'Output Image dimensions:  '+strtrim(rx,2)+' '+strtrim(ry,2)+aa
endif

caviar_reproj_u,polera,poledec,699L,im,rawim,et,cmat,rpj_mnrad,rpj_mxrad,mnlon,mxlon,pixscale,nl,$
ry,rx,rzoom, $
reprojected_image=rpi, raw_reprojected_image=rrpi, zoom_reprojected_image=zrpi, interp=interp, light_time=light_time, silent=silent, reproj_xygrid=reproj_xygrid, cubic=cubic, sinc=sinc


return
end
