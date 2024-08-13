
	pro pointing_ra_dec_u, cmat, RA, dec


	rho_cam = [0.0d0,0.0d0,1.0d0]

;	rho_J2000 = cmat#rho_cam

	cspice_mtxv,cmat,rho_cam,rho_J2000

	cspice_recrad,rho_J2000,range,RA,dec

	if RA lt 0.0d0 then RA=RA+(2.0d0*!dpi)

	RA = RA*360.0d0/(2.0d0*!dpi)
	dec = dec*360.0d0/(2.0d0*!dpi)


	;RA=atan(rho_J2000[1],rho_J2000[0])
	;if RA lt 0.0d0 then RA=RA+(2.0d0*!dpi)
	;r2=(rho_J2000[0]*rho_J2000[0])+(rho_J2000[1]*rho_J2000[1])
	;r=sqrt(r2)
	;dec=atan(rho_J2000[2],r)
	;RA = RA*360.0d0/(2.0d0*!dpi)
	;dec = dec*360.0d0/(2.0d0*!dpi)

	return
	end
	      
