
	pro qmul_ucac2_access,epoch,RA,dec,size,mag1,mag2,stars

	path=getenv("UCAC2_CAT")

	cmd='qmul_ucac2_access ,'+path+','+string(epoch)+','+string(RA)+','+string(dec)+$
	','+string(size)+','+string(mag1)+','+string(mag2)+','

	print,cmd

	spawn,cmd,ucac2_output

	n_stars=n_elements(ucac2_output)

	if ucac2_output(0) eq '' then begin
		pstring='Number of UCAC2 stars = 0'
	        print,pstring
		stars=[[99999999L,99999999L],[0L,0L],[0L,0L],[5000L,5000L]]
		return
	endif

	stars=lonarr(n_stars,4)

	for i=0,n_stars-1 do begin
	
	print,ucac2_output[i]

	p=strsplit(ucac2_output[i],' ')

	name=double(strmid(ucac2_output[i],p[0],p[1]-p[0]))
	ras=double(strmid(ucac2_output[i],p[1],p[2]-p[1]))
	decs=double(strmid(ucac2_output[i],p[2],p[3]-p[2]))
	mags=double(strmid(ucac2_output[i],p[3],strlen(ucac2_output[i])-p[3]))

	stars[i,0]=round(name)
	stars[i,1]=round(ras)
	stars[i,2]=round(decs)
	stars[i,3]=round(mags)

	endfor

	pstring='Number of UCAC2 stars = '+strtrim(string(n_stars),2)
	print,pstring

	position=where (stars[*,3] le (mag2*100.0d0))

	if position[0] ne -1 then begin
		reduced_stars=stars[position,*]
		stars=reduced_stars
	endif

	return
	end
