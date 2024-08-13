
	pro man_find_satellites,y,x,im,et,nl,planet,planet_coords,y_cent,x_cent,located_satellites,$
			cam_params,cmat,sat_name

	num_objects=n_elements(planet[*,0])

	separation=sqrt((y-planet_coords[*,0])^2.0d0+(x-planet_coords[*,1])^2.0d0)

	min_sep=min(separation,min_sub)

	object=planet[min_sub,0]
			
	print,planet[min_sub,0],planet_coords[min_sub,0],planet_coords[min_sub,1]

		cspice_spkez,long(planet[min_sub,0]),et,'J2000','CN+S',-82L,state,ltime
		rho=state[0:2]
		distance=norm(rho)
		cspice_bodvar,long(planet[min_sub,0]),'RADII',radii
		max_radius=max(radii)
		radius_pixel=atan(max_radius,distance)/cam_params[11]

		print,planet[min_sub,0]
		print,'radius=',max_radius
		print,'distance=',distance	
		print,'radius (pixels)=',radius_pixel

		if radius_pixel gt 0.5 then begin
			fwhm=2.0d0*radius_pixel
		endif else begin
			fwhm=1.0d0
		endelse

		print,'fwhm=',fwhm

		x_min=x-round(fwhm*2.0)-1
		x_max=x+round(fwhm*2.0)+1
		y_min=y-round(fwhm*2.0)-1
		y_max=y+round(fwhm*2.0)+1

		;x_min=x-round(fwhm*1.0)-1
		;x_max=x+round(fwhm*1.0)+1
		;y_min=y-round(fwhm*1.0)-1
		;y_max=y+round(fwhm*1.0)+1

		if x_min lt 0 then x_min=0
		if x_max gt (nl-1) then x_max=nl-1
		if y_min lt 0 then y_min=0
		if y_max gt (nl-1) then y_max=nl-1		


		sub_im=im[x_min:x_max,y_min:y_max]
		;sub_im=im[x-round(fwhm*2.0)-1:x+round(fwhm*2.0)+1,y-round(fwhm*2.0)-1:y+round(fwhm*2.0)+1]
		;sub_im modified due to crashes for large objects near edge of frame
		flux=0.0
		find_stars,sub_im,xf,yf,flux,sharp,round,2.0,fwhm,[-1.0,1.0],[0.2,1.0]

		;having a problem with more than one local maxima for a satellite
		;going with highest flux

		if n_elements(flux) gt 1 then begin
			lfm=max(flux,lfmmax)
			xf=xf(lfmmax)
			yf=yf(lfmmax)
		endif

		;y_cent=(1023.0d0-yf)+(y-round(fwhm*2.0)-1)
		;x_cent=xf+(x-round(fwhm*2.0)-1)
		y_cent=(1023.0d0-yf)+y_min; modified to account for edge of frame problem
		x_cent=xf+x_min; modified to account for edge of frame problem
	
	print,y_cent,x_cent

		for i=0,num_objects-1 do begin
			if double(planet[min_sub,0]) eq located_satellites[i,0] then begin
				located_satellites[i,7]=y_cent
				located_satellites[i,8]=x_cent
				p2radec,cam_params,cmat,nl,y_cent,x_cent,RA,dec
				print,strcompress(string(fix(located_satellites[i,0])),/remove_all),RA,dec
				located_satellites[i,5]=RA*3600.0d3
				located_satellites[i,6]=dec*3600.0d3
			endif
		endfor

	sat_name=fix(planet[min_sub,0])

	return
	end	

