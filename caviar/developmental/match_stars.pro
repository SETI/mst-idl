	pro match_stars,coords,found_star_coords,stars,located_stars,min_dist

	num=n_elements(stars[*,0])

	temp_located_stars=dblarr(num,8)

	num=n_elements(stars[*,0])

	j=0

	for i=0,num-1 do begin
 	
	distance_line=coords[i,0]-found_star_coords[*,0]
   	distance_sample=coords[i,1]-found_star_coords[*,1]
   	distance=(distance_line*distance_line)+$
	       (distance_sample*distance_sample)
   	distance=sqrt(distance)

	minimum=min(distance,minimum_subscript)

	if minimum le min_dist then begin

		temp_located_stars[j,0]=stars[i,0]
		temp_located_stars[j,1]=stars[i,1]	
		temp_located_stars[j,2]=stars[i,2]
		temp_located_stars[j,3]=stars[i,3]
		temp_located_stars[j,4]=found_star_coords[minimum_subscript,0]
		temp_located_stars[j,5]=found_star_coords[minimum_subscript,1]
		temp_located_stars[j,6]=coords[i,0]	
		temp_located_stars[j,7]=coords[i,1]

		j=j+1
	endif

	endfor

        if j eq 0 then located_stars=-1 else begin
	  located_stars=temp_located_stars[0:j-1,*]
        endelse

	return
	end	
