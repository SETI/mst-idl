
	pro star_names,stars,names

	n_stars=n_elements(stars[*,0])

	names=strarr(n_stars,1)

	for i=0,n_stars-1 do begin

	if stars[i,0] ge 0 then begin
		 names[i]="UCAC2-"+strcompress(string(long(stars[i,0])),/remove_all)
	endif else begin
	         name_string=+strcompress(string(-1*long(stars[i,0])),/remove_all)
		 name_length=strlen(name_string)
		 if name_length eq 8 then name_string='0'+name_string
 		 if name_length eq 7 then name_string='00'+name_string
		 if name_length eq 6 then name_string='000'+name_string
		 if name_length eq 5 then name_string='0000'+name_string
		 if name_length eq 4 then name_string='0000'+name_string
		 names[i]="TYCHO2-"+strmid(name_string,0,4)+'.'$
		          +strmid(name_string,4,5)
	endelse

	endfor

	return
	end