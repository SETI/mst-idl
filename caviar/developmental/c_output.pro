
	pro c_output,label,image_name,located_stars,located_satellites,cmat,cam_offset_quat

	pointing_label=strmid(image_name,0,13)+'.CPF'

	openw,lun,pointing_label,/get_lun

	printf,lun,image_name
	printf,lun,label[*,78]
	num=n_elements(located_stars[*,0])

	for i=0,num-1 do begin
		

	if located_stars[i,0] ge 0 then begin
		 names="UCAC2-"+strcompress(string(long(located_stars[i,0])),/remove_all)
	endif else begin
	         name_string=+strcompress(string(-1*long(located_stars[i,0])),/remove_all)
		 name_length=strlen(name_string)
		 if name_length eq 8 then name_string='0'+name_string
 		 if name_length eq 7 then name_string='00'+name_string
		 if name_length eq 6 then name_string='000'+name_string
		 if name_length eq 5 then name_string='0000'+name_string
		 if name_length eq 4 then name_string='0000'+name_string
		 names="TYCHO2-"+strmid(name_string,0,4)+'.'$
		          +strmid(name_string,4,5)
	endelse
		printf,lun,names,located_stars[i,4],located_stars[i,5]
	endfor

	num=n_elements(located_satellites[*,0])

	for i=0,num-1 do begin
		if (located_satellites[i,0] ge 610) and (located_satellites[i,0] le 618) then begin
		if (located_satellites[i,7] ne -1.0) and (located_satellites[i,7] ne -1.0) then begin
		satellitestring=long(located_satellites[i,0])
		satellitename=strcompress(satellitestring,/remove_all)
		printf,lun,'      ',satellitename,'       ',located_satellites[i,7],' ',located_satellites[i,8]
		endif
		endif
	endfor


	printf,lun,format='(d15.11,d15.11,d15.11,d15.11)',cmat[*,0]
	printf,lun,format='(d15.11,d15.11,d15.11,d15.11)',cmat[*,1]
	printf,lun,format='(d15.11,d15.11,d15.11,d15.11)',cmat[*,2]
	
	printf,lun,'Produced at Queen Mary, University of London under the direction'
	printf,lun,'of Dr. C.D. Murray. For enquiries contact M.W.Evans@qmul.ac.uk or'
	printf,lun,'K.Beurle@qmul.ac.uk.' 

	free_lun,lun

	return
	end
