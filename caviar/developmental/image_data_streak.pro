
	pro image_data_streak,label,ets,epoch,exposure,cam_name,pmats,nl,found

        sclk = where( label[0,*] eq 'SPACECRAFT_CLOCK_STOP_COUNT', count )
        if count eq 1 then sclk=sclk[0] else stop, sclk
        exposure = where( label[0,*] eq 'EXPOSURE_DURATION', count )
        if count eq 1 then exposure=exposure[0] else stop, exposure
        cam_name = where( label[0,*] eq 'INSTRUMENT_ID', count )
        if count eq 1 then cam_name=cam_name[0] else stop, cam_name
        nl = where( label[0,*] eq 'NL', count )
        if count eq 1 then nl=nl[0] else stop, nl

	sclk = label[1,sclk];76]
	exposure = label[1,exposure];29]
	exposure = double(exposure)/1000.0d0
	cam_name =label[1,cam_name];63]
	nl=long(label[1,nl]);8])

	sc=-82L

	cspice_scencd,sc,sclk,sclkdp
	cspice_sct2e,sc,sclkdp,et 
	f=1.d0-findgen(9)/8.
	ets=et-(exposure*f) 
;	sclkdps=dblarr(9)
;	et=et-(exposure*0.5d0)
	pmats=dblarr(9,3,3)

	for i=0,8 do begin
	cspice_sce2t, sc, ets(i), sclkdps
	cspice_ckgp,-82000L,sclkdps,1000.0d0,'J2000',pmat,clkout,found
	pmats(i,*,*)=pmat
	end	

	cspice_timout,ets(4),'YYYY-DOYTHR:MN:SC.###::UTC',21,ctime

	year=strmid(ctime,0,4)
	doy=strmid(ctime,5,3)
	
	doy=double(doy)
	year=double(year)
	epoch=year+(doy/365.25)

	return
	end
