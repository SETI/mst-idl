function radwander,file
	nterms=6
	restore, file
;rrpi=rebin(rrpi(0:2999,*),3000/4.,n_elements(rrpi(0,*)))
	sx=size(rproj_l_start)
	if sx(1) ne 0 then begin
		mnrad=min([rproj_r_i,rproj_r_o])
		mxrad=max([rproj_r_i,rproj_r_o])
		mxlon=max([rproj_l_start,rproj_l_stop])
		mnlon=min([rproj_l_start,rproj_l_stop])
		end		
	sr=size(rrpi)
	ls=sr(1)
	rs=sr(2)
	radi=findgen(rs)/(rs-1.)*(mxrad-mnrad)+mnrad
	long=findgen(ls)/(ls-1.)*(mxlon-mnlon)+mnlon
	out=fltarr(1+2*nterms,ls)
	out(0,*)=long
	rmi=133500
	rmo=133650
;	rmi=133550
;	rmo=133650
;	rii=133450
;	rio=133550
	rii=133425
	rio=133575
	roi=133700
	roo=133750

	for i=0,ls-1 do begin
	 data=rrpi(i,*) 
	 qi=where(radi gt rii and radi lt rio and data ne 0)
	 qm=where(radi gt rmi and radi lt rmo and data ne 0)
	 qo=where(radi gt roi and radi lt roo and data ne 0)
	 if n_elements(qi) gt 7 then begin
		ai=[0.005,133480,10,0,0,0]
		gi=gaussfit(radi(qi),data(qi),ai,nterms=nterms)
		out(1:nterms,i)=ai
		end
	 if n_elements(qm) gt 10 then begin
		am=[0.01,133586,10,0,0,0]
		gm=gaussfit(radi(qm),data(qm),am,nterms=nterms)
	 	out(nterms+1:2*nterms,i)=am
		end
;	 if n_elements(qo) gt 10 then begin
;		ao=[0.01,133710,10,0,0,0]
;		go=gaussfit(radi(qo),data(qo),ao,nterms=nterms)
;	 	out(2*nterms+1:3*nterms,i)=ao
;		end
	 end
	return, out
	end

