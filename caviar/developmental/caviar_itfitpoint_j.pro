	pro caviar_itfitpoint_j,rastart,decstart,twiststart,located_stars,vobs,cam_params,tol,rao,deco,twisto,cmat,nl,notwist=notwist,debug=debug


	working=1

	while(working) do begin

	rac=rastart
	decc=decstart
	twistc=twiststart

	print,' '
	print,format = '(d16.13,d16.13,d16.13)',rac,decc,twistc
	print,' '

        if keyword_set(notwist) then begin
	  get_pointing_notwist_j,rac,decc,twistc,located_stars,vobs,cam_params,cmat,nl,debug=debug
        endif else begin
	  get_pointing,rac,decc,twistc,located_stars,vobs,cam_params,cmat,nl
        endelse 

	num=n_elements(located_stars[*,0])
	count=0
	temp_res_located_stars=located_stars

	resd=sqrt(((located_stars[*,6]-located_stars[*,4])*(located_stars[*,6]-located_stars[*,4]))+$
	((located_stars[*,7]-located_stars[*,5])*(located_stars[*,7]-located_stars[*,5])))

	maxresd=max(resd,max_subscript)

	for i=0,num-1 do begin
		if resd[i] ne maxresd and maxresd gt tol then begin
		temp_res_located_stars[count,*]=located_stars[i,*]
		count=count+1
		endif
	endfor

	if count ne 0 then begin
		final_located_stars=temp_res_located_stars[0:count-1,*]
		located_stars=final_located_stars
	endif

	if count eq 0 then working=0

	end	

	rao=rac
	deco=decc
	twisto=twistc

	return
	end
