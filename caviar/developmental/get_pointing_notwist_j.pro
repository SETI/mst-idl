
	pro get_pointing_notwist_j,rac,decc,twistc,located_stars,vobs,cam_params,cmat,nl,debug=debug

        nn = 1331; 121;1331

	rdta=dblarr(3,nn)
	rms=dblarr(nn)

	ra=dblarr(11)
	dec=dblarr(11)
	;twist=dblarr(11)

	num=n_elements(located_stars[*,0])

	if cam_params[0] gt 1000.0 then fc=1.0d0
	if cam_params[0] lt 500.0 then fc=10.0d0

	working=1
	old_pixel=-1

	while (working) do begin

	ra[0]=rac-((5.0d0*fc*6.0d-6)/cos(decc))
	ra[1]=rac-((4.0d0*fc*6.0d-6)/cos(decc))
	ra[2]=rac-((3.0d0*fc*6.0d-6)/cos(decc))
	ra[3]=rac-((2.0d0*fc*6.0d-6)/cos(decc))
	ra[4]=rac-(fc*6.0d-6/cos(decc))
	ra[5]=rac
	ra[6]=rac+(fc*6.0d-6/cos(decc))
	ra[7]=rac+((2.0d0*fc*6.0d-6)/cos(decc))
	ra[8]=rac+((3.0d0*fc*6.0d-6)/cos(decc))
	ra[9]=rac+((4.0d0*fc*6.0d-6)/cos(decc))
	ra[10]=rac+((5.0d0*fc*6.0d-6)/cos(decc))

	dec[0]=decc-(5.0d0*fc*6.0d-6)
	dec[1]=decc-(4.0d0*fc*6.0d-6)
	dec[2]=decc-(3.0d0*fc*6.0d-6)
	dec[3]=decc-(2.0d0*fc*6.0d-6)
	dec[4]=decc-(fc*6.0d-6)
	dec[5]=decc
	dec[6]=decc+(fc*6.0d-6)
	dec[7]=decc+(fc*2.0d0*6.0d-6)
	dec[8]=decc+(fc*3.0d0*6.0d-6)
	dec[9]=decc+(fc*4.0d0*6.0d-6)
	dec[10]=decc+(fc*5.0d0*6.0d-6)

	;twist[0]=twistc-(5.0d0*1.0d-3)
	;twist[1]=twistc-(4.0d0*1.0d-3)
	;twist[2]=twistc-(3.0d0*1.0d-3)
	;twist[3]=twistc-(2.0d0*1.0d-3)
	;twist[4]=twistc-(1.0d-3)
	;twist[5]=twistc
	;twist[6]=twistc+(1.0d-3)
	;twist[7]=twistc+(2.0d0*1.0d-3)
	;twist[8]=twistc+(3.0d0*1.0d-3)
	;twist[9]=twistc+(4.0d0*1.0d-3)
	;twist[10]=twistc+(5.0d0*1.0d-3)

	i=0
	print,' '
	print,' '
	;print,i


	for j=0,10 do begin
	for k=0,10 do begin
	for l=0,10 do begin


	rdta[0,i]=ra[j]
	rdta[1,i]=dec[k]
	rdta[2,i]=twistc	;twist[l]

	i=i+1
	;print,i
	endfor
	endfor
	endfor

	for i=0,nn-1 do begin

	cspice_eul2m,rdta[2,i],((0.5d0*!dpi)-rdta[1,i]),((0.5d0*!dpi)+rdta[0,i]),3,1,3,cmat
	;;;Rob's fix
	cmat=-1.0d*cmat
	image_coords_j,located_stars[*,0:2],cmat,vobs,cam_params,nl,coords
	cmat=-1.0d*cmat
	residuals=coords[*,0:1]-located_stars[*,4:5]
	distance=(residuals[*,0]*residuals[*,0])+(residuals[*,1]*residuals[*,1])
	distance1=total(distance)
        if num eq 1 then rms[i]=sqrt(distance1) else begin
	  rms[i]=sqrt(distance1/(1.0d0*num*(num-1.0d0)))
        endelse

	endfor

	minimum_rms=min(rms,min_subscript)

	rac=rdta[0,min_subscript]
	decc=rdta[1,min_subscript]
	;twistc=rdta[2,min_subscript]

	print,min_subscript,rms[min_subscript]

	print,format = '(d16.13,d16.13,d16.13)',rac,decc,twistc

	pixel=min_subscript

	if pixel eq old_pixel then working=0
	old_pixel=pixel
	
	end

	print,' '
	print,minimum_rms
	print,' '


	working=1
	old_pixel=-1

	print,format = '(d16.13,d16.13,d16.13)',rac,decc,twistc

	while (working) do begin

	ra[0]=rac-((fc*5.0d0*6.0d-7)/cos(decc))
	ra[1]=rac-((fc*4.0d0*6.0d-7)/cos(decc))
	ra[2]=rac-((fc*3.0d0*6.0d-7)/cos(decc))
	ra[3]=rac-((fc*2.0d0*6.0d-7)/cos(decc))
	ra[4]=rac-(fc*6.0d-7/cos(decc))
	ra[5]=rac
	ra[6]=rac+(fc*6.0d-7/cos(decc))
	ra[7]=rac+((fc*2.0d0*6.0d-7)/cos(decc))
	ra[8]=rac+((fc*3.0d0*6.0d-7)/cos(decc))
	ra[9]=rac+((fc*4.0d0*6.0d-7)/cos(decc))
	ra[10]=rac+((fc*5.0d0*6.0d-7)/cos(decc))

	dec[0]=decc-(fc*5.0d0*6.0d-7)
	dec[1]=decc-(fc*4.0d0*6.0d-7)
	dec[2]=decc-(fc*3.0d0*6.0d-7)
	dec[3]=decc-fc*(2.0d0*6.0d-7)
	dec[4]=decc-(fc*6.0d-7)
	dec[5]=decc
	dec[6]=decc+(fc*6.0d-7)
	dec[7]=decc+(fc*2.0d0*6.0d-7)
	dec[8]=decc+(fc*3.0d0*6.0d-7)
	dec[9]=decc+(fc*4.0d0*6.0d-7)
	dec[10]=decc+(fc*5.0d0*6.0d-7)

	;twist[0]=twistc-(5.0d0*0.3d-3)
	;twist[1]=twistc-(4.0d0*0.3d-3)
	;twist[2]=twistc-(3.0d0*0.3d-3)
	;twist[3]=twistc-(2.0d0*0.3d-3)
	;twist[4]=twistc-0.3d-3
	;twist[5]=twistc
	;twist[6]=twistc+0.3d-3
	;twist[7]=twistc+(2.0d0*0.3d-3)
	;twist[8]=twistc+(3.0d0*0.3d-3)
	;twist[9]=twistc+(4.0d0*0.3d-3)
	;twist[10]=twistc+(5.0d0*0.3d-3)

	i=0

	for j=0,10 do begin
	for k=0,10 do begin
	for l=0,10 do begin

	rdta[0,i]=ra[j]
	rdta[1,i]=dec[k]
	rdta[2,i]=twistc;twist[l]

	i=i+1

	endfor
	endfor
	endfor

	for i=0,nn-1 do begin

	cspice_eul2m,rdta[2,i],((0.5d0*!dpi)-rdta[1,i]),((0.5d0*!dpi)+rdta[0,i]),3,1,3,cmat
	;;;Rob's fix
	cmat=-1.0d*cmat
	image_coords_j,located_stars[*,0:2],cmat,vobs,cam_params,nl,coords
	cmat=-1.0d*cmat
	residuals=coords[*,0:1]-located_stars[*,4:5]
	distance=(residuals[*,0]*residuals[*,0])+(residuals[*,1]*residuals[*,1])
	distance1=total(distance)
        if num eq 1 then rms[i]=sqrt(distance1) else begin
	  rms[i]=sqrt(distance1/(1.0d0*num*(num-1.0d0)))
        endelse

	endfor

	minimum_rms=min(rms,min_subscript)


	rac=rdta[0,min_subscript]
	decc=rdta[1,min_subscript]
	;twistc=rdta[2,min_subscript]

	print,min_subscript

	print,format = '(d16.13,d16.13,d16.13)',rac,decc,twistc

	pixel=min_subscript

	if pixel eq old_pixel then working=0
	old_pixel=pixel
	
	end

	print,' '
	print,minimum_rms
	print,' '

	working=1
	old_pixel=-1

	print,format = '(d16.13,d16.13,d16.13)',rac,decc,twistc

	while (working) do begin

	ra[0]=rac-((fc*5.0d0*6.0d-8)/cos(decc))
	ra[1]=rac-((fc*4.0d0*6.0d-8)/cos(decc))
	ra[2]=rac-((fc*3.0d0*6.0d-8)/cos(decc))
	ra[3]=rac-((fc*2.0d0*6.0d-8)/cos(decc))
	ra[4]=rac-(fc*6.0d-8/cos(decc))
	ra[5]=rac
	ra[6]=rac+(fc*6.0d-8/cos(decc))
	ra[7]=rac+((fc*2.0d0*6.0d-8)/cos(decc))
	ra[8]=rac+((fc*3.0d0*6.0d-8)/cos(decc))
	ra[9]=rac+((fc*4.0d0*6.0d-8)/cos(decc))
	ra[10]=rac+((fc*5.0d0*6.0d-8)/cos(decc))

	dec[0]=decc-(fc*5.0d0*6.0d-8)
	dec[1]=decc-(fc*4.0d0*6.0d-8)
	dec[2]=decc-(fc*3.0d0*6.0d-8)
	dec[3]=decc-(fc*2.0d0*6.0d-8)
	dec[4]=decc-(fc*6.0d-8)
	dec[5]=decc
	dec[6]=decc+(fc*6.0d-8)
	dec[7]=decc+(fc*2.0d0*6.0d-8)
	dec[8]=decc+(fc*3.0d0*6.0d-8)
	dec[9]=decc+(fc*4.0d0*6.0d-8)
	dec[10]=decc+(fc*5.0d0*6.0d-8)

	;twist[0]=twistc-(5.0d0*0.1d-3)
	;twist[1]=twistc-(4.0d0*0.1d-3)
	;twist[2]=twistc-(3.0d0*0.1d-3)
	;twist[3]=twistc-(2.0d0*0.1d-3)
	;twist[4]=twistc-0.1d-3
	;twist[5]=twistc
	;twist[6]=twistc+0.1d-3
	;twist[7]=twistc+(2.0d0*0.1d-3)
	;twist[8]=twistc+(3.0d0*0.1d-3)
	;twist[9]=twistc+(4.0d0*0.1d-3)
	;twist[10]=twistc+(5.0d0*0.1d-3)

	i=0

	for j=0,10 do begin
	for k=0,10 do begin
	for l=0,10 do begin

	rdta[0,i]=ra[j]
	rdta[1,i]=dec[k]
	rdta[2,i]=twistc;twist[l]

	i=i+1

	endfor
	endfor
	endfor

	for i=0,nn-1 do begin

	cspice_eul2m,rdta[2,i],((0.5d0*!dpi)-rdta[1,i]),((0.5d0*!dpi)+rdta[0,i]),3,1,3,cmat
	;;;; Rob's fix
	cmat=-1.0d*cmat
	image_coords_j,located_stars[*,0:2],cmat,vobs,cam_params,nl,coords
	cmat=-1.0d*cmat
	residuals=coords[*,0:1]-located_stars[*,4:5]
	distance=(residuals[*,0]*residuals[*,0])+(residuals[*,1]*residuals[*,1])
	distance1=total(distance)
        if num eq 1 then rms[i]=sqrt(distance1) else begin
	  rms[i]=sqrt(distance1/(1.0d0*num*(num-1.0d0)))
        endelse

	endfor

	minimum_rms=min(rms,min_subscript)


	rac=rdta[0,min_subscript]
	decc=rdta[1,min_subscript]
	;twistc=rdta[2,min_subscript]

	print,min_subscript

	print,format = '(d16.13,d16.13,d16.13)',rac,decc,twistc

	pixel=min_subscript

	if pixel eq old_pixel then working=0
	old_pixel=pixel
	
	end

	print,' '
	print,minimum_rms
	print,' '

	cspice_eul2m,rdta[2,min_subscript],((0.5d0*!dpi)-rdta[1,min_subscript]),$
	((0.5d0*!dpi)+rdta[0,min_subscript]),3,1,3,cmat
	;;; Rob's fix
	cmat=-1.0d*cmat
	image_coords_j,located_stars[*,0:2],cmat,vobs,cam_params,nl,coords
	cmat=-1.0d*cmat	
	final_residuals=located_stars[*,4:5]-coords
	rms_line_residuals=total(final_residuals[*,0]*final_residuals[*,0])
	rms_sample_residuals=total(final_residuals[*,1]*final_residuals[*,1])
        if num eq 1 then begin
	  rms_line_residuals=sqrt(rms_line_residuals)
	  rms_sample_residuals=sqrt(rms_sample_residuals)
        endif else begin
	  rms_line_residuals=sqrt(rms_line_residuals/(1.0d0*num*(num-1.0d0)))
	  rms_sample_residuals=sqrt(rms_sample_residuals/(1.0d0*num*(num-1.0d0)))
        endelse

	located_stars[*,6:7]=coords[*,0:1]

	print,' '
	print,' '
	print,'    STAR     ','           line           sample','           line           sample','     line residuals','   sample residuals'
	print,'                             centroided                        fitted                  (centroided-fitted)'
	print,' '
	for i=0,num-1 do begin
		starstring=string(long(located_stars[i,0]))
		if long(starstring) gt 0 then starname='UCAC2-'+strcompress(starstring,/remove_all)
		if long(starstring) lt 0 then starname='TYCHO2'+strcompress(starstring,/remove_all)
		print,starname,located_stars[i,4],located_stars[i,5],coords[i,0],coords[i,1],$
		      located_stars[i,4]-coords[i,0],located_stars[i,5]-coords[i,1]
	endfor

	print,' '
	print,'rms residual (pixels)',minimum_rms
	print,'rms line residual (pixels)',rms_line_residuals
	print,'rms sample residual (pixels)',rms_sample_residuals
	print,' '
        if keyword_set(debug) then stop

	return
	end
	
