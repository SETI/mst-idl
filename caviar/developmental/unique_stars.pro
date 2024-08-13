
	pro unique_stars,tyc2_mag,stars_in,stars_out

	stars_out=stars_in

	nstars=n_elements(stars_in[*,0])
	rd=dblarr(nstars,2)
	rho=dblarr(nstars,3)
	r=dblarr(nstars,1)
	rhoi=dblarr(3)
	rhoj=dblarr(3)

	sep_array=dblarr(nstars,nstars)


	rd[*,0]=stars_in[*,1]*(2.0d0*!dpi)/(3600.0d3*360.0d0)
	rd[*,1]=stars_in[*,2]*(2.0d0*!dpi)/(3600.0d3*360.0d0)

	r[*,0]=1.0d0*cos(rd[*,1])
	rho[*,0]=r[*,0]*cos(rd[*,0])
	rho[*,1]=r[*,0]*sin(rd[*,0])
	rho[*,2]=sin(rd[*,1])/1.0d0

	for i=0,nstars-1 do begin
	for j=0,nstars-1 do begin

		rhoi[0]=rho[i,0]
		rhoi[1]=rho[i,1]
		rhoi[2]=rho[i,2]
		rhoj[0]=rho[j,0]
		rhoj[1]=rho[j,1]
		rhoj[2]=rho[j,2]
		sep_array[j,i]=cspice_vsep(rhoi,rhoj)

	endfor
	endfor

	count=0L

	for i=0,nstars-1 do begin

		match=where(sep_array(i,*) lt 6.0d-6)

		if n_elements(match) eq 1 then begin 
			stars_out[count,*]=stars_in[i,*]
			count=count+1
		endif


		if n_elements(match) eq 2 then begin
			if stars_in[i,0] gt 0 and stars_in[match[0],0] lt 0 then begin
				if (stars_in[match[0],3]/100.0d0) gt tyc2_mag then begin
					stars_out[count,*]=stars_in[i,*]
					count=count+1
				endif
			endif
			if stars_in[i,0] gt 0 and stars_in[match[1],0] lt 0 then begin
				if (stars_in[match[1],3]/100.0d0) gt tyc2_mag then begin
					stars_out[count,*]=stars_in[i,*]
					count=count+1
				endif
			endif
		endif

		if n_elements(match) eq 2 then begin
			if stars_in[i,0] lt 0 and stars_in[match[0],0] gt 0 then begin
				if (stars_in[i,3]/100.0d0) le tyc2_mag then begin
					stars_out[count,*]=stars_in[i,*]
					count=count+1
				endif
			endif
			if stars_in[i,0] lt 0 and stars_in[match[1],0] gt 0 then begin
				if ((stars_in[i,3])/100.0d0) le tyc2_mag then begin
					stars_out[count,*]=stars_in[i,*]
					count=count+1
				endif
			endif
		endif


	endfor

	stars_out=stars_out[0:count-1,*]

	return
	end
			
	