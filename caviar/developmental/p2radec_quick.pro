
	pro p2radec_quick,cam_params,cmat,nl,line,sample,RA,dec

	rho=dblarr(3)
	rho_J2000=dblarr(3)
	rho_cam=dblarr(3)
	radec_array=dblarr(21,21,2)
	residual=dblarr(21,21)
	line_i=-1000.0d0
	sample_i=-1000.0d0
	acc=0.1d0

	if nl eq 1024 then summ=1.0d0
	if nl eq 512 then summ=2.0d0
	if nl eq 256 then summ=4.0d0

	if summ eq 1.0d0 then boresight=[511.5d0,511.5d0]
	if summ eq 2.0d0 then boresight=[255.5d0,255.5d0]
	if summ eq 4.0d0 then boresight=[127.5d0,127.5d0]

	rho[0]=(sample-boresight[0])/(cam_params[0]*cam_params[4]/summ)
	rho[1]=(line-boresight[1])/(cam_params[0]*cam_params[7]/summ)
	rho[2]=1.0d0

	rho=rho/(norm(rho))

	tcmat=transpose(cmat)

	d_rho_J2000=tcmat##rho
	rho_J2000[0:2]=d_rho_J2000[0,0:2]

	cspice_recrad,rho_J2000,range,RA,dec

	for i=-10,10 do begin
	for j=-10,10 do begin
		radec_array[j+10,i+10,0]=((i*6.0d-6*acc)/cos(dec))+RA
		radec_array[j+10,i+10,1]=(j*6.0d-6*acc)+dec
	endfor
	endfor

	for i=0,20 do begin
	for j=0,20 do begin

		r=1.0d0*cos(radec_array[j,i,1])
		rho_J2000[0]=r*cos(radec_array[j,i,0])
		rho_J2000[1]=r*sin(radec_array[j,i,0])
		rho_J2000[2]=(sin(radec_array[j,i,1]))/1.0d0

		d_rho_cam=cmat##rho_J2000
		rho_cam[0:2]=d_rho_cam[0,0:2]

		x=(cam_params[0]/rho_cam[2])*rho_cam[0]
		y=(cam_params[0]/rho_cam[2])*rho_cam[1]
		
		r2=(x*x)+(y*y)

		deltax=((x*r2)*cam_params[1])+((x*y)*cam_params[2])+((x*x)*cam_params[3])
		deltay=((y*r2)*cam_params[1])+((y*y)*cam_params[2])+((x*y)*cam_params[3])

		sample_i=(cam_params[4]/summ*(x+deltax))+(cam_params[5]/summ*(y+deltay))+((-1.0d0+(nl*1.0d0))/2.0d0)
		line_i=(cam_params[6]/summ*(x+deltax))+(cam_params[7]/summ*(y+deltay))+((-1.0d0+(nl*1.0d0))/2.0d0)

		residual[j,i]=sqrt((sample-sample_i)^2.0+(line-line_i)^2.0)

	endfor
	endfor

	minimum=min(residual,min_subscript)

	i_min=long(min_subscript/21.0)
	j_min=min_subscript-(i_min*21L)

	RA=(radec_array[j_min,i_min,0]*360.0d0)/(2.0d0*!dpi)
	dec=(radec_array[j_min,i_min,1]*360.0d0)/(2.0d0*!dpi)

	if (i_min eq 0) or (i_min eq 20) then print,'***ERROR***' and stop 
	if (j_min eq 0) or (j_min eq 20) then print,'***ERROR***' and stop

	return
	end

