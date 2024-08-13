
	pro image_coords_u, stars, cmat, vobs, pixscale, nl, coords, x=x, y=y

	num_stars=n_elements(stars[*,0])

	rho_J2000=dblarr(num_stars,3)

	coords=dblarr(num_stars,2)

	RA=stars[*,1]/3600.0d3
	dec=stars[*,2]/3600.0d3

        ; Note that rho_J2000 is the same as the rho_ring that is the product
        ; of get_ring.pro, except that it is normalized to have magnitude 1.
	r=1.0d0*cos(2.0d0*!dpi*dec/360.0d0)
	rho_J2000[*,0]=r*cos(2.0d0*!dpi*RA/360.0d0)
	rho_J2000[*,1]=r*sin(2.0d0*!dpi*RA/360.0d0)
	rho_J2000[*,2]=(sin(2.0d0*!dpi*dec/360.0d0))/1.0d0

        corrected_rho_J2000 = rho_J2000
	if (where(vobs ne 0))[0] ne -1 then begin
            for i=0l,num_stars-1 do begin
		cspice_stelab,reform(rho_J2000[i,*]),vobs,crj2000
                corrected_rho_J2000[i,*] = crj2000
            endfor
        endif

	rho_cam=cmat##corrected_rho_J2000
	
;	if nl eq 1024 then summ=1.0d0
;	if nl eq 512 then summ=2.0d0
;	if nl eq 256 then summ=4.0d0

	x=rho_cam[*,0]/rho_cam[*,2]/pixscale+nl/2
	y=rho_cam[*,1]/rho_cam[*,2]/pixscale+nl/2
	

;	x=(cam_params[0]/rho_cam[*,2])*rho_cam[*,0]
;	y=(cam_params[0]/rho_cam[*,2])*rho_cam[*,1]

;        ; Relative to optical center of distortion
 ;       xoc = x - cam_params[8]/cam_params[4]*summ
  ;      yoc = y - cam_params[9]/cam_params[4]*summ
		
;	r2=(xoc*xoc)+(yoc*yoc)

;	deltax=((xoc*r2)*cam_params[1])+((xoc*yoc)*cam_params[2])+((xoc*xoc)*cam_params[3])
;	deltay=((yoc*r2)*cam_params[1])+((yoc*yoc)*cam_params[2])+((xoc*yoc)*cam_params[3])

;	sample=((cam_params[4]/summ)*(x+deltax))+((cam_params[5]/summ)*(y+deltay))+((-1.0d0+(nl*1.0d0))/2.0d0)
;	line=((cam_params[6]/summ)*(x+deltax))+((cam_params[7]/summ)*(y+deltay))+((-1.0d0+(nl*1.0d0))/2.0d0)

	coords[*,0]=y
	coords[*,1]=x
        ;if num_stars gt 1000 then stop  ; For debugging

	return
	end
	      
