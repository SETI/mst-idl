
	pro caviar_reproj,polera,poledec,planet_id,im,rawim,et,$
                   cmat,ri,ro,lon_start,lon_stop,cam_params,nl,rpi_l,rpi_s,zf,$
                          reprojected_image=reprojected_image, $
                          raw_reprojected_image=raw_reprojected_image, $
                          zoom_reprojected_image=zoom_reprojected_image, $
                          interp=interp, light_time=light_time, silent=silent, $
                          reproj_xygrid=reproj_xygrid, cubic=cubic, sinc=sinc

        if not keyword_exists(interp) then interp=1
        if keyword_set(sinc) then interp=0
        if not keyword_exists(cubic) then cubic=0;-0.5
	reprojected_image=bytarr(round(rpi_s),round(rpi_l))
	raw_reprojected_image=lonarr(round(rpi_s),round(rpi_l))
        reproj_xygrid=fltarr(round(rpi_s),round(rpi_l),2)
	if (size(rawim))[3] eq 4 then raw_reprojected_image=float(raw_reprojected_image)

	reprojected_image=byte(0)*reprojected_image
	raw_reprojected_image=0L*raw_reprojected_image

	r_range=ro-ri
	r_step=r_range/(1.0d0*round(rpi_l-1))

	if lon_stop lt lon_start then d_lon_start=lon_start-360.0d0

	r_radius=dblarr(round(rpi_l))

	for i=0,round(rpi_l)-1 do begin

		if not keyword_set(silent) then begin
                   if i mod fix(rpi_l/20) eq 0 then print, i, ' / ', rpi_l
		endif
		r_radius[i]=(i*r_step)+ri
		get_ring,et,r_radius[i],lon_start,lon_stop,polera,poledec,round(rpi_s),ring,699L, light_time=light_time

		image_coords,ring,cmat,[0.0d0,0.0d0,0.0d0],cam_params,nl,coords

                newmethod = 1
                if keyword_set(interp) then begin
                    ; This is the one that is used by default. 
                    ; Interpolate surrounding pixels to get reproj value
                    ; Also (new 5/22/08 MST) keep track of line and sample
                    reproj_xygrid[*,i,0] = coords[*,1]
                    reproj_xygrid[*,i,1] = coords[*,0]
                    raw_reprojected_image[*,i] = interpolate( rawim, coords[*,1], coords[*,0], missing=0, cubic=cubic )
                    ; Use bilinear interpolation regardless for rpi
                    reprojected_image[*,i] = interpolate( im, coords[*,1], coords[*,0], missing=0 )
                endif else if keyword_set(sinc) then begin
                    reproj_xygrid[*,i,0] = coords[*,1]
                    reproj_xygrid[*,i,1] = coords[*,0]
                    good = coords ge 0 and coords le nl-1
                    good = good[*,0] and good[*,1]
                    for j=0,round(rpi_s)-1 do if good[j] then begin
                        raw_reprojected_image[j,i] = sint2d( coords[j,1], coords[j,0], rawim, delta=30 )
                    endif
                    ; Use bilinear interpolation regardless for rpi
                    reprojected_image[*,i] = interpolate( im, coords[*,1], coords[*,0], missing=0 )
                endif else if keyword_set(newmethod) then begin
                    inimage = where( round(coords[*,0]) ge 0 and round(coords[*,0]) le 1023 and round(coords[*,1]) ge 0 and round(coords[*,1]) le 1023, count )
                    if count gt 0 then begin
                        reprojected_image[inimage,i] = im[ round(coords[inimage,1]), round(coords[inimage,0]) ]
                        raw_reprojected_image[inimage,i] = rawim[ round(coords[inimage,1]), round(coords[inimage,0]) ]
                    endif
                endif else for j=0,round(rpi_s)-1 do begin

		if (round(coords[j,0])) ge 0 and (round(coords[j,0]) le 1023) then begin
		if (round(coords[j,1])) ge 0 and (round(coords[j,1]) le 1023) then begin		
			reprojected_image[j,i]=im[round(coords[j,1]),round(coords[j,0])]
			raw_reprojected_image[j,i]=rawim[round(coords[j,1]),round(coords[j,0])]
		endif
		endif

		endfor
	
	endfor

;	zoom_reprojected_image=bytarr((round(zf)*round(rpi_s)),(round(zf)*round(rpi_l)))
;
;	for i=0,(round(zf)*round(rpi_s))-1 do begin
;	for j=0,(round(zf)*round(rpi_l))-1 do begin
;		zoom_reprojected_image[i,j]=reprojected_image[fix(i/round(zf)),fix(j/round(zf))]
;	endfor
;	endfor
zoom_reprojected_image = rebin( reprojected_image, (round(zf)*round(rpi_s)), (round(zf)*round(rpi_l)), /sample )		

;	;window,3,xsize=800,ysize=800
;	window,3,xsize=(round(zf)*round(rpi_s)),ysize=(round(zf)*round(rpi_l))
;
;	tv,zoom_reprojected_image

	return
	end
