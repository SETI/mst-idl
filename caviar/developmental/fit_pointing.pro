
	pro fit_pointing,rai,deci,twisti,located_stars,vobs,foc,residuals,rao,deco,twisto


	createjac,rai,deci,twisti,located_stars,vobs,foc,J,residuals

	input=[rai,deci,twisti]

	num=n_elements(located_stars[*,0])

	choldc,J,p,/double
	adjust=cholsol(J,p,residuals,/double)

;	svdc,J,w,u,v

;	adjust=svsol(u,w,v,residuals,/double)

	output=input-adjust

	rao=output[0]
	deco=output[1]
	twisto=output[2]

	if rao lt 0.0d0 then rao=rao+(2.0d0*!dpi)
	if twisto lt 0.0d0 then twisto=twisto+(2.0d0*!dpi)

	stop

	return
	end


