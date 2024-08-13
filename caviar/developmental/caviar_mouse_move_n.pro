
	pro caviar_mouse_move_n,im,symbol,symbol_size,stars,planet,ring,cmat,vobs,$
		       pixscale,coords,vobs_planet,planet_coords,$
		       ring_symbol,ring_symbol_size,ring_coords,s_names,$
                       planet_symbol,planet_symbol_size,nl,plan_names

	stop=dblarr(3)

	wset,1
	tv,im,/order
	x = round(coords[*,1])
	y = round(coords[*,0])
	@plot_stars
	xp = round(planet_coords[*,1])
	yp = round(planet_coords[*,0])
	get_planet_name,planet[*,0],plan_names
	@plot_planets
	xr = round(ring_coords[*,1])
	yr = round(ring_coords[*,0])
	@plot_rings

	working=1

	while (working) do begin

	cursor, xmouse_start, ymouse_start, /down,/device

	if(!mouse.button EQ 4 or !mouse.button EQ 16) then working=0

	while (!mouse.button eq 1 ) do begin

		cursor,xmouse,ymouse,/change,/device

		x_move=xmouse-xmouse_start
		y_move=ymouse-ymouse_start

                xmouse_start=xmouse
                ymouse_start=ymouse

		cspice_m2eul,cmat,3,1,3,ang3,ang2,ang1
		twiststart=ang3
		decstart=((0.5d0*!dpi)-ang2)
		rastart=(ang1-(0.5d0*!dpi))
		if rastart lt 0.0d0 then rastart=rastart+(2.0d0*!dpi)
		if twiststart lt 0.0d0 then twiststart=twiststart+(2.0d0*!dpi)

		twist=twiststart+(!dpi/2.0d0)

		rot=[[cos(-twist),sin(-twist),0.0d0],[-sin(-twist),cos(-twist),0.0d0],[0.0,0.0,1.0d0]]

		start=[y_move,x_move,20.0d0]
		start=start*1.0d0

		d_stop=rot##start
		stop[0:2]=d_stop[0,0:2]

		dec=decstart+stop[1]*pixscale;cam_params[11]

		ra=rastart+(stop[0]*pixscale/cos(dec))

		rastart=ra
		decstart=dec

		cspice_eul2m,twiststart,((0.5d0*!dpi)-dec),((0.5d0*!dpi)+ra),3,1,3,cmat

		image_coords_u,stars,cmat,vobs,pixscale,nl,coords
		image_coords_u,planet,cmat,vobs_planet,pixscale,nl,planet_coords
		image_coords_u,ring,cmat,vobs_planet,pixscale,nl,ring_coords

		tv,im,/order
		x = round(coords[*,1])
		y = round(coords[*,0])
		@plot_stars
		xp = round(planet_coords[*,1])
		yp = round(planet_coords[*,0])
		get_planet_name,planet[*,0],plan_names
		@plot_planets
		xr = round(ring_coords[*,1])
		yr = round(ring_coords[*,0])
		@plot_rings


	end

	end

	return
	end
