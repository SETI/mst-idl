
	pro qmul_u2access,pathu,newep,ra,dec,w,mag1,mag2,output_cutdown

	newep=double(newep)
	ra=double(ra)
	dec=double(dec)
	w=double(w)

	output=intarr(30000,4)
	output=output*1L
	nss=1L

	pathu=strcompress(pathu,/REMOVE_ALL)

	istat=call_external(getenv("UCAC2_SHARE"),'qmul_u2access',pathu,newep,ra,dec,w,mag1,mag2,output,nss)

;	output_cutdown=intarr(nss,4)
	output_cutdown=output[0:(nss-1)>0,*]

	return
	end

;****************************************************
	pro open_zfile,pathz,un,zn,only_rd

	pathz=strcompress(pathz,/REMOVE_ALL)

	if only_rd eq 'T' then only_rd_in=1L
	if only_rd eq 'F' then only_rd_in=0L
	
	istat=call_external(getenv("UCAC2_SHARE"),'open_zfile',pathz,un,zn,only_rd_in)

	return
	end
;*****************************************************
