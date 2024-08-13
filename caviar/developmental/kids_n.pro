pro kids_n,et,cmat,pixscale,nl,sc,polera,poledec,im,vobs_planet,radius_inner,$
	radius_outer,lon_start,lon_stop,redraw=redraw

	wset,1

if keyword_set(redraw) then begin
  redraw = 0
  radius_1 = radius_inner
  radius_2 = radius_outer
  lon_1 = lon_start
  lon_2 = lon_stop
  goto, donewithmouse
endif
again:

	cursor,x,y,2,/device
	y=nl-1-y

	case !MOUSE.button of

4:	goto, done

1:	begin

	line=y
	sample=x

	p2radec_quicker_u,pixscale,cmat,nl,line,sample,RA,dec
	p2ralon_n,cmat,et,polera,poledec,sc,RA,dec,radius_1,lon_1

	get_ring,et,[radius_1],0.0,360.0,polera,poledec,3600,temp_reproj_region_i,8L,-32L
	get_ring,et,[60000,5.0d0*radius_1],lon_1,lon_1,polera,poledec,1,sline_far_end,8L, -32L

	image_coords_u,temp_reproj_region_i,cmat,vobs_planet,pixscale,nl,temp_reproj_region_i_coords
	image_coords_u,sline_far_end,cmat,vobs_planet,pixscale,nl,sline_far_end_coords

	plots,round(temp_reproj_region_i_coords[*,1]),(nl-1)-round(temp_reproj_region_i_coords[*,0]),psym=3,symsize=0.5,$
		color=make_array(n_elements(temp_reproj_region_i_coords[*,1]),value=green()),/device
	plots,[round(sline_far_end_coords[0,1]),round(sline_far_end_coords[1,1])],$
		(nl-1)-[round(sline_far_end_coords[0,0]),round(sline_far_end_coords[1,0])],$
		linestyle=0,color=green(),/device

	endcase
else:	begin	
	goto,again
	endcase

endcase

done: 

onceagain:

	cursor,x,y,2,/device
	y=nl-1-y

	case !MOUSE.button of

4:	goto, done_2

1:	begin

	tv,im,/order

	line=y
	sample=x

	p2radec_quicker_u,pixscale,cmat,nl,line,sample,RA,dec
	p2ralon_n,cmat,et,polera,poledec,sc,RA,dec,radius_2,lon_2



donewithmouse:



	get_ring,et,[radius_1,radius_2],lon_1,lon_2,polera,poledec,3600,reproj_region,8L,-32L

	image_coords_u,reproj_region,cmat,vobs_planet,pixscale,nl,reproj_region_coords
	reproj_region_symbol=-3
	reproj_region_symbol_size=0.5

	plots,round(reproj_region_coords[0:3599,1]),(nl-1)-round(reproj_region_coords[0:3599,0]),psym=reproj_region_symbol,$
	symsize=reproj_region_symbol_size,color=make_array(n_elements(reproj_region[0:3599,0]),value=green()),/device

	plots,round(reproj_region_coords[3600:7199,1]),(nl-1)-round(reproj_region_coords[3600:7199,0]),psym=reproj_region_symbol,$
	symsize=reproj_region_symbol_size,color=make_array(n_elements(reproj_region[3600:7199,0]),value=green()),/device

	plots,round([reproj_region_coords[0,1],reproj_region_coords[3600,1]]),(nl-1)-round([reproj_region_coords[0,0],$
	reproj_region_coords[3600,0]]),linestyle=0,color=green(),/device

	plots,round([reproj_region_coords[3599,1],reproj_region_coords[7199,1]]),(nl-1)-round([reproj_region_coords[3599,0],$
	reproj_region_coords[7199,0]]),linestyle=0,color=green(),/device


        endcase

16:     begin
        goto, again
        endcase
else:	begin
	goto, onceagain
	endcase
endcase
	goto, onceagain

done_2:
	if radius_2 lt radius_1 then begin
		radius_inner=radius_2
		radius_outer=radius_1
	endif else begin 
		radius_inner=radius_1
		radius_outer=radius_2
	endelse
	lon_start=lon_1
	lon_stop=lon_2

	return

	end



