	pro dont_match_stars,stars,coords,planet,planet_coords,found_object_coords,flux,$
                             unknown_objects,min_dist,cam_params,cmat,et,nl,polera,poledec,sc

	;known_objects=[stars[*,0:2],planet]
	known_objects=stars[*,0:2]
	;known_coords=[coords,planet_coords]
	known_coords=coords
;currently only looking to match stars and not satellites

	num=n_elements(found_object_coords[*,0])

	temp_unknown_objects=dblarr(num,5)
	
	j=0

	for i=0,num-1 do begin
 	
	distance_line=found_object_coords[i,0]-known_coords[*,0]
   	distance_sample=found_object_coords[i,1]-known_coords[*,1]
   	distance=(distance_line*distance_line)+$
	       (distance_sample*distance_sample)
   	distance=sqrt(distance)

	minimum=min(distance,minimum_subscript)

	if minimum gt min_dist then begin

		temp_unknown_objects[j,0]=found_object_coords[i,0]	
		temp_unknown_objects[j,1]=found_object_coords[i,1]
		temp_unknown_objects[j,2]=flux[i]
		p2radec_quick,cam_params,cmat,nl,found_object_coords[i,0],found_object_coords[i,1],RA,dec
		p2ralon,cmat,et,polera,poledec,sc,RA,dec,radius,lon
		temp_unknown_objects[j,3]=radius
		temp_unknown_objects[j,4]=lon
		j=j+1
	endif

	endfor

	unknown_objects=temp_unknown_objects[0:j-1,*]

	return
	end	