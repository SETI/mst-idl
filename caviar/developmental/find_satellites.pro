
	pro find_satellites,im,et,planet,pixel_sub_ang,vobs_planet,cmat,cam_params,planet_coords,located_satellites

	num_sats=n_elements(planet[*,0])-1

	count=0L

	num_planet=n_elements(planet[*,0])

	temp_located_satellites=dblarr(num_planet,5)
	temp_located_satellites[0,0]=0

	for i=0,num_sats-1 do begin

	print,planet[(i+1),0],planet_coords[(i+1),0],planet_coords[(i+1),1]

		if (planet_coords[i+1,0] ge 10.5) and (planet_coords[i+1,0]) le 1013.5 then begin
		if (planet_coords[i+1,1] ge 10.5) and (planet_coords[i+1,1]) le 1013.5 then begin

		cspice_spkez,long(planet[(i+1),0]),et,'J2000','CN+S',-82L,state,ltime
		rho=state[0:2]
		distance=norm(rho)
		cspice_bodvar,long(planet[(i+1),0]),'RADII',radii
		max_radius=max(radii)
		radius_pixel=atan(max_radius,distance)/pixel_sub_ang

		print,planet[(i+1),0]
		print,'radius=',max_radius
		print,'distance=',distance	
		print,'radius (pixels)=',radius_pixel

		if radius_pixel gt 0.5 then begin
			fwhm=2.0d0*radius_pixel
		endif else begin
			fwhm=1.0d0
		endelse

		search_radius=long(0.5*fwhm)+10

		print,'search radius=',search_radius
		print,'fwhm=',fwhm		

		samplelow=round(planet_coords[(i+1),1])-search_radius
		samplehigh= round(planet_coords[(i+1),1])+search_radius
		linelow=round(planet_coords[(i+1),0])-search_radius
		linehigh= round(planet_coords[(i+1),0])+search_radius

		;print,' '
		;print,'linelow=',linelow
		;print,'linehigh=',linehigh
		;print,'samplelow=',samplelow
		;print,'samplehigh=',samplehigh
		;print,' '

		sub_im=im[samplelow:samplehigh,linelow:linehigh]

		;window,xsize=2*search_radius,ysize=2*search_radius
		;tv,sub_im,/order

		flux=0.0
		find_stars,sub_im,xf,yf,flux,sharp,round,2.0,fwhm,[-1.0,1.0],[0.2,1.0]

		numelements=n_elements(xf)
		if (n_elements(xf) eq 1) and (max(flux) gt 0.0) then begin
			temp_located_satellites[count,0]=planet[(i+1),0]
			temp_located_satellites[count,1]=linelow+1023-yf
			temp_located_satellites[count,2]=samplelow+xf
			temp_located_satellites[count,3]=planet[(i+1),1]
			temp_located_satellites[count,4]=planet[(i+1),2]
			count=count+1		
		endif

		endif
		endif

	endfor

	located_satellites=temp_located_satellites[0:(count-1),*]

	return
	end	
