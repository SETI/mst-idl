
	pro get_planet,et,planet,props=props

;        planet_codes = [ 699L, lindgen(30)+601, 632L, 633L, 634L, 635L, 698L, 65060L, $
;                        1L, 2L, 3L, 4L, 5L, 7L, 8L ]
        planet_codes = [ 699L, lindgen(30)+601, 632L, 633L, 634L, 635L, 649L, 653L, $
                         1L, 2L, 3L, 4L, 5L, 7L, 8L ]
        if keyword_set(props) then planet_codes = [ planet_codes, 699001 ]
        npl = n_elements(planet_codes)
	planet=lonarr(npl,3)

        for j=0,npl-1 do begin
	  cspice_spkez,planet_codes[j],et,'J2000','CN+S',-82l,state,ltime
	  rho=state[0:2]
	  cspice_recrad,rho,range,RA,dec
	  RA=3600.0d3*RA*360.0d0/(2.0d0*!dpi)
	  dec=3600.0d3*dec*360.0d0/(2.0d0*!dpi)
	  planet[j,1]=RA
	  planet[j,2]=dec
	  planet[j,0]=planet_codes[j]
	endfor

	return
	end
