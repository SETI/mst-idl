; Converted from get_ring.pro by MST 6/21/08
; Using cylindrical coordinates, specify r and z, evaluate at input ansalon (degrees)

	pro get_rz,et,radius,startz,stopz,polera,poledec,n_points,ring,planet_id, sc, state=state, zs=zs, light_time=light_time, J2000_xyz=J2000_xyz, ansalon=ansalon

        if keyword_set(light_time) then print, 'NOTE:  get_rz.pro no longer uses the input keyword light_time.'
	if not keyword_set(sc) then sc = -82L

	tstopz=stopz

;        foo = where( tstopz lt startz, count )
;	if count gt 0 then tstopz[foo] = tstopz[foo] + 360.0

	z_range=tstopz-startz

        zstep=z_range/(n_points-1)
        foo = where( z_range eq 0.0, count )
        if count gt 0 then zstep[foo] = 0

        if keyword_set(zs) then begin
            n_rings = 1
            n_points = n_elements(zs)
        endif else begin
            n_rings=n_elements(radius)
        endelse

	ring=lonarr(n_rings*n_points,3)

        ; capJ is the colatitude of Saturn's pole in J2000
        ; capN is 90 plus the longitude of Saturn's pole in J2000
	capN=(polera+90.0d0)*2.0d0*!dpi/360.0
	capJ=(90.0d0-poledec)*2.0d0*!dpi/360.0

	rot=dblarr(3,3)

        ; Create a matrix which transforms from a Saturn coordinate system
        ; to J2000.  In the former, Saturn's pole is the z-axis, while
        ; the x-axis is the (ascending) intersection between Saturn's 
        ; equatorial plane and that of J2000.
        ; First rotate about the x-axis by angle J (the pole's colatitude), 
        ; so that Saturn's pole is at the J2000 latitude, but its longitude
        ; is still along the negative y-axis.
        ; Then rotate about the new z-axis by angle N (90 degrees to move
        ; the pole to the positive x-axis, and then further by the J2000
        ; longitude), so that Saturn's pole is in its J2000 position.
	rot[0,0]=cos(capN)
	rot[1,0]=-sin(capN)*cos(capJ)
	rot[2,0]=sin(capN)*sin(capJ)
	rot[0,1]=sin(capN)
	rot[1,1]=cos(capN)*cos(capJ)
	rot[2,1]=-cos(capN)*sin(capJ)
	rot[0,2]=0.0
	rot[1,2]=sin(capJ)
	rot[2,2]=cos(capJ)

	xyz=dblarr(n_points,3)

;        cspice_spkez,planet_id,et,'J2000','CN+S',sc,state,ltime
        cspice_spkez,planet_id,et,'J2000','NONE',sc,state,ltime
        rho_planet = rotate( state[0:2], 1 )
        vel_planet = rotate( state[3:5], 1 )

	for j=1l,n_rings do begin

                if keyword_set(zs) then begin
                    xyz[*,0] = radius*cos(ansalon*!dpi/180)
                    xyz[*,1] = radius*sin(ansalon*!dpi/180)
                    xyz[*,2] = zs
                endif else begin
                    ; startz and zstep should never have >1 element,
                    ; so convert them to scalars in case needed.
                    ring_z = startz[0] + (lindgen(n_points)*zstep[0])
                    xyz[*,0]=radius[j-1]*cos(ansalon*!dpi/180)
                    xyz[*,1]=radius[j-1]*sin(ansalon*!dpi/180)
                    xyz[*,2]=ring_z
                endelse

		J2000_xyz=rot##xyz
                _rho_planet = rebin(rho_planet,n_points,3)
                rho_ring = _rho_planet + J2000_xyz
                ltime = v_mag(rho_ring)/299792.458d0
;                _rho_planet = _rho_planet - $
;                              rebin(vel_planet,n_points,3)*rebin(ltime,n_points,3)

                for i=0,n_elements(ltime)-1 do begin
;                    cspice_spkez,planet_id,et-ltime[i],'J2000','CN+S',sc,state,ltime_dummy
                    cspice_spkez,planet_id,et-ltime[i],'J2000','NONE',sc,state,ltime_dummy
                    _rho_planet[i,*] = state[0:2]
                endfor
                rho_ring = _rho_planet + J2000_xyz

        	;cspice_recrad,rho_ring,range,RA,dec
                radec = cart_to_polar( rho_ring )
                RA = radec[*,1]
                dec = radec[*,0]
                int = lindgen(n_points) + (j-1)*n_points
        	ring[int,1]=3600.0d3*RA*360.0d0/(2.0d0*!dpi)
        	ring[int,2]=3600.0d3*dec*360.0d0/(2.0d0*!dpi)

	endfor

	return
	end

