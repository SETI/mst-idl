FUNCTION phasearray, filename, rarray=rarray, larray=larray, rs=rs, $
	xarray=xarray, yarray=yarray
	 image_name=filename
	 @caviar_loadonly2
	 
	cspice_spkez,-82L,et,'J2000','NONE',699L,state,ltime
	camera_coord = rotate(state[0:2],1)
	
	cspice_spkez,10L,et,'J2000','NONE',699L,state,ltime
	sun_coord = rotate(state[0:2],1)

	out=dblarr(nl,nl)
	if not keyword_set(rs) then rs=2^3
	outp=dblarr(nl/rs+1,nl/rs+1)
	rar=outp
	lar=outp
	for i=0,nl/rs do begin
	for j=0,nl/rs do begin
	aimpoint=[i*rs,j*rs]
	p2radec,cam_params,cmat,nl,aimpoint[1],aimpoint[0],aimp_ra,aimp_dec
	p2ralon,cmat,et,polera,poledec,sc,aimp_ra,aimp_dec,aimp_radius,aimp_lon
	get_ring,et,aimp_radius,0,0,polera,poledec,1,0,699L,lons=aimp_lon,j2000_xyz=aimpoint_coord
	a=camera_coord-aimpoint_coord
	b=sun_coord-aimpoint_coord
	outp(i,j)=acos(total(a*b,2)/sqrt(total(a^2,2)*total(b^2,2)))*180.0/!dpi
	rar(i,j)=aimp_radius
	lar(i,j)=aimp_lon
	end
	end
out=interpolate(outp,findgen(nl)/1./rs,findgen(nl)/1./rs, /grid, cubic=-.5)	
rarray=interpolate(rar,findgen(nl)/1./rs,findgen(nl)/1./rs, /grid, cubic=-.5)	
xarray=interpolate(rar*cos(lar*!dpi/180.),findgen(nl)/1./rs,$
	findgen(nl)/1./rs, /grid, cubic=-.5)	
yarray=interpolate(rar*sin(lar*!dpi/180.),findgen(nl)/1./rs,$
	findgen(nl)/1./rs, /grid, cubic=-.5)	
larray=interpolate(lar,findgen(nl)/1./rs,findgen(nl)/1./rs, /grid, cubic=-.5)	

return, out
end
