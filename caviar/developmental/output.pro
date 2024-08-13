
	pro output,label,image_name,located_stars,located_satellites,cmat,cam_offset_quat

	pointing_label=strmid(image_name,0,13)+'.QMPL'

	openu,lun,pointing_label,/get_lun,append=1
	;openu,lun,pointing_label,/get_lun

	spawn,'date',date
	who=getenv('USER')
	host=getenv('HOST')
	printf,lun,'Prepared by ',who
	printf,lun,'Hostname ',host
	printf,lun,date

	printf,lun,' '
	printf,lun,'IMAGE DATA FILE '+image_name
	printf,lun,label[*,69]
	printf,lun,label[*,67]
	printf,lun,label[*,8]
	printf,lun,label[*,74]
	printf,lun,label[*,75]
	printf,lun,label[*,76]
	printf,lun,label[*,77]
	printf,lun,label[*,78]
	printf,lun,label[*,29]
	printf,lun,' '
	printf,lun,'                  catalogue location (corrected for proper 
        printf,lun,'                    motion but not aberration & parallax)'
	printf,lun,'    STAR               RA           dec','
	printf,lun,' '
	num=n_elements(located_stars[*,0])
	for i=0,num-1 do begin
		starstring=string(long(located_stars[i,0]))
		starname='UCAC2-'+strcompress(starstring,/remove_all)
		printf,lun,format='(a14,d14.9,d14.9)',starname,located_stars[i,1]/3600.0d3,located_stars[i,2]/3600.0d3
	endfor	



	printf,lun,' '
	printf,lun,'    STAR          line     sample     line     sample   line residuals sample residuals'
	printf,lun,'                     centroided          fitted               (centroided-fitted)'
	printf,lun,' '

	num=n_elements(located_stars[*,0])

	for i=0,num-1 do begin
		starstring=string(long(located_stars[i,0]))
		starname='UCAC2-'+strcompress(starstring,/remove_all)
		printf,lun,format='(a14,d10.4,d10.4,d10.4,d10.4,d10.4,d10.4)',starname,located_stars[i,4],located_stars[i,5],located_stars[i,6],located_stars[i,7],$
		      located_stars[i,4]-located_stars[i,6],located_stars[i,5]-located_stars[i,7]
	endfor

	;printf,lun,' '
	;printf,lun,'rms residual (pixels)',minimum_rms
	;printf,lun,'rms line residual (pixels)',rms_line_residuals
	;printf,lun,'rms sample residual (pixels)',rms_sample_residuals
	;printf,lun,' '

	num=n_elements(located_satellites[*,0])

	printf,lun,' '
	printf,lun,'   SATELLITE      line     sample        RA            dec'
	printf,lun,'                (centroided-c.o.l.)'
	for i=0,num-1 do begin
		satellitestring=long(located_satellites[i,0])
		satellitename=strcompress(satellitestring,/remove_all)
		rasat=(located_satellites[i,3]/3600.0d3)
		decsat=(located_satellites[i,4]/3600.0d3)
		printf,lun,format='(a14,d10.4,d10.4,d14.9,d14.9)',satellitename,located_satellites[i,1],located_satellites[i,2],$
                                  rasat,decsat
	endfor

  	cspice_m2eul,cmat,3,1,3,twisto,deco,rao

	rao=rao-(0.5d0*!dpi)
	deco=(0.5d0*!dpi)-deco

	rao=rao*360.0d0/(2.0d0*!dpi)
	deco=deco*360.0d0/(2.0d0*!dpi)
	twisto=twisto*360.0d0/(2.0d0*!dpi)

	if rao lt 0.0d0 then rao=rao+360.0d0
	if twisto lt 0.0d0 then twisto=twisto+360.0d0

	printf,lun,' '
	printf,lun,'         RA            dec          twist'
	printf,lun,format='(d15.10,d15.10,d15.10)',rao,deco,twisto
	printf,lun,' '

	cspice_m2q,cmat,cmat_quat

	printf,lun,'                Pointing quaternion'
	printf,lun,format='(d14.11,d14.11,d14.11,d14.11)',cmat_quat
	printf,lun,' '

	printf,lun,'              Camera offset quaternion'
	printf,lun,format='(d14.11,d14.11,d14.11,d14.11)',cam_offset_quat
	printf,lun,' '

	free_lun,lun

	return
	end