function edgerad,file
	nterms=6
	restore, file
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
	rei=133300;133350
	reo=133500;133450
	reei=133600
	reeo=133800

	for i=0,ls-1 do begin
	 data=deriv(rrpi(i,*)) 
	 qi=where(radi gt rei and radi lt reo and data ne 0)
         qo=where(radi gt reei and radi lt reeo and data ne 0)
         if n_elements(qi)  gt 10 then begin
	       ai=[-.01,133423,10,0,0,0]
                gi=gaussfit(radi(qi),data(qi),ai,nterms=nterms)
                out(1:nterms,i)=ai
                end
         if n_elements(qo) gt 10  then begin
		am=[.01,133745,10,0,0,0]
                gm=gaussfit(radi(qo),data(qo),am,nterms=nterms)
                out(nterms+1:2*nterms,i)=am
                end


	 end
	return, out
	end

