
	pro qmul_tycho2_access,epoch,RA,dec,size,stars

	radius=3600.0d0*size/2.0
	radius=strcompress(string(radius),/remove_all)

	cmd='scat -c ty2 -dr '+radius+' '+strcompress(string(RA),/remove_all)$
	    +' '+strcompress(string(dec),/remove_all)+' J2000 -y '+$
            strcompress(string(epoch),/remove_all)

	spawn,cmd,tycho_output

	n_stars=n_elements(tycho_output)

	stars=lonarr(n_stars,4)

	for i=0,n_stars-1 do begin

	p=strsplit(tycho_output[i],' ')

	name=1.0d5*double(strmid(tycho_output[i],p[0],p[1]))
	ras=3600.0d3*double(strmid(tycho_output[i],p[1],p[2]))
	decs=3600.0d3*double(strmid(tycho_output[i],p[2],p[3]))
	mags=100*double(strmid(tycho_output[i],p[3],p[4]))
	stars[i,0]=-1*round(name)
	stars[i,1]=round(ras)
	stars[i,2]=round(decs)
	stars[i,3]=round(mags)

	endfor

	return
	end
