FUNCTION zrarray, filename, rs=rs
	 image_name=filename
	 @caviar_loadonly2
	 
	cspice_spkez,-82L,et,'J2000','NONE',699L,state,ltime
	camera_coord = rotate(state[0:2],1)
	
	cspice_spkez,10L,et,'J2000','NONE',699L,state,ltime
	sun_coord = rotate(state[0:2],1)

	out=dblarr(2,nl,nl)
	if not keyword_Set(rs) then rs=2^3
	zz=dblarr(nl/rs+1,nl/rs+1)
	rr=dblarr(nl/rs+1,nl/rs+1)
	for i=0,nl/rs do begin
	for j=0,nl/rs do begin
	aimpoint=[i*rs,j*rs]
	p2radec,cam_params,cmat,nl,aimpoint[1],aimpoint[0],aimp_ra,aimp_dec
	p2raz_quicker,cmat,et,polera,poledec,sc,aimp_ra,aimp_dec,aimp_r,aimp_z
	zz(i,j)=aimp_z
	rr(i,j)=aimp_r
	end
	end
out(0,*,*)=interpolate(rr,findgen(nl)/1./rs,findgen(nl)/1./rs, /grid, cubic=-.5)	
out(1,*,*)=interpolate(zz,findgen(nl)/1./rs,findgen(nl)/1./rs, /grid, cubic=-.5)	
return, out
end
