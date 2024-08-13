;;
;; jschmidt Mon Nov  5 11:51:33 EET 2012
;;
;;
;;
;@PlanetaryData

;----------------------------------------------------
function gentab,n,min_in,max_in,double=double,openmax=openmax,log=log,power=power

	on_error,2

	min=min_in
	max=max_in

	n=long(n(0))
	if n eq 1 then begin

		tab=min

	endif else begin

		nnorm=n-1
		if keyword_set(openmax) then nnorm=n

		if keyword_set(log) then begin


			  if min le 0 then message, $
				'LOG SCALE ONLY FOR POSITIVE DATA POINTS'
			  if max le 0 then message, $
				'LOG SCALE ONLY FOR POSITIVE DATA POINTS'

			min=alog(min)
			max=alog(max)

		endif

			min=double(min)
			max=double(max)
			tab=dindgen(n)/double(nnorm)*(max-min)+min
	
		if keyword_set(log) then tab=exp(tab)
		if keyword_set(power) then begin 
			apu=(tab-min)/(max-min)
			apu=apu^(1d0/double(power))
			tab=apu*(max-min)+min
		endif

	endelse

return,tab
end

;----------------------------------------------------
function strim,x,fo=fo
on_error,2
;returns a string without space
;print,x
ff=''
if keyword_set(fo) then ff="("+fo+")"
y = strtrim(string(x,f=ff),2)
  return,y
end

;----------------------------------------------------
function get_E_Anomaly,aa,ee,rr,fa_in
;
; determine the eccentric anomaly
;
	; first shift a copy of the true anomaly to the positive domain,
	; in case it is negative
	fa=fa_in
	while fa lt 0 do fa=fa+!dpi*2d0
	while fa gt (!dpi*2d0)  do fa=fa-!dpi*2d0

	; compute the eccentric anomaly
	cos_ea=(1d0-rr/aa)/ee
	; first solution for the cosine inversion
	ea=acos(cos_ea)

	; check if true anomaly and eccentric anomaly are in the same
	; half-ellipse
	modulof=floor(fa/!dpi)+1
	moduloe=floor(ea/!dpi)+1

	if modulof ne moduloe then begin
	; if not, use the second solution for the cosine inversion
		ea=!dpi*2d0-ea
		moduloe=floor(ea/!dpi)+1
	endif

	; if the initial true anomaly was negative, then shift the
	; solution for the eccentric anomaly also to the negative domain
	if fa_in lt 0 then ea=ea-!dpi*2d0

return,ea
end

;----------------------------------------------------
function newton_func,xx
;
; newton iteration to solve two equations for eccentricity and pericenter longitude
;
	common newton_par,r1,r2,saturn_gm,fix_cap_e

	ee=xx(0)
	pomega=xx(1)

	; semi-major axis is given by two nodal radii and eccentricity
	aa=2d0*r1*r2/(r1+r2)/(1d0-ee*ee)

	; the spread of the footpoint of the second impact
	delta_a=100d3
	; the duration of the passage of the swarm of projectiles
	; at the second impact
	dt=35d0
	; the time span between the two impacts
	tt=2d0/3d0*dt*aa/delta_a

	;first equation
	cosp=cos(pomega)
	ecosp=ee*cosp
	zero1=(1d0-r1/r2)/(1d0+r1/r2)-ecosp

	;second equation
	f1=-pomega
	f2=-pomega+!dpi
	E1=get_E_Anomaly(aa,ee,r1,f1)
	E2=get_E_Anomaly(aa,ee,r2,f2)
	nn=sqrt(saturn_gm/aa/aa/aa)
	zero2=E2-E1+(1-2*fix_cap_e)*ee*(sin(E2)-sin(E1))-nn*tt
	
return,[zero1,zero2]
end

;----------------------------------------------------
;------------------- MAIN ---------------------------
;----------------------------------------------------
pro GetElementsOfSwarm,pomega0=pomega0,fix_cap_e=_fix_cap_e

	tek_color

	!p.charsize=1.5
	thick=2
	if !d.name eq 'PS' then begin
		thick=4
	endif else begin
		device,decomposed=0,retain=2
	endelse

	!x.thick=thick	
	!y.thick=thick	
	!p.thick=thick	

	!except=0
	common newton_par,r1,r2,saturn_gm,fix_cap_e
        if keyword_exists(_fix_cap_e) then begin
          fix_cap_e=_fix_cap_e
        endif else fix_cap_e=1

;	dat=PlanetaryData()
;	gm=dat.saturn.gm
	saturn_gm=3.7936672d+16

	; radius of second impact, A1 and A2
	r2=130d6
	; a range for possible radii of the first impact
	r1tab=gentab(10,80d6,140d6)

	; tables for eccentricity, semi-major axis, pericentre angle
	nr=n_elements(r1tab)
	etab=dblarr(nr)
	atab=dblarr(nr)
	ptab=dblarr(nr)

	; initial values [eccentricity, pericentre angle]
	p0=!dpi*1.5d0
	e0=0.8d0
	if keyword_set(pomega0) then p0=pomega0
	xx=[e0,p0]

	; high precision goal
	tol=1d-15

	; main loop
	for ir=0,nr-1 do begin

		r1=r1tab(ir)
		xx=newton(xx,'newton_func',/double,tolx=tol, $
			tolf=tol,itmax=500000L,tolmin=tol)
		etab(ir)=xx(0)
		pomega=xx(1)
		while pomega lt 0 do pomega=pomega+!dpi*2d0
		while pomega gt (!dpi*2d0) do pomega=pomega-!dpi*2d0
		ptab(ir)=pomega

		print,newton_func(xx)

	endfor

	; semi-major axis
	atab=2d0*r1tab*r2/(r1tab+r2)/(1d0-etab*etab)

;
; plotting
;
	!p.multi=[0,2,2]

	window,/free	
	!x.title='R1 [kkm]'
	plot,r1tab/1d6,atab*(1d0+etab)/1d6,/nod, $
		ytitle='radius [kkm]',ylog=0
	xyouts,100,165,/data,'a(1+e)'
	xyouts,90,100,/data,'a'
	xyouts,100,73,/data,'a(1-e)'

	oplot,r1tab/1d6,atab/1d6,psym=-4,col=2
	oplot,r1tab/1d6,atab*(1d0-etab)/1d6,psym=-5,col=3
	oplot,r1tab/1d6,atab*(1d0+etab)/1d6,psym=-6,col=4
	oplot,r1tab/1d6,replicate(137d6,nr)/1d6,lin=2
	;xyouts,min(r1tab/1d6)*1.01,120d6*1.01/1d6,'RINGS'
;	oplot,r1tab/1d6,replicate(dat.iapetus.a,nr)/1d6,lin=2
;	xyouts,min(r1tab/1d6)*1.01,dat.iapetus.a*1.01/1d6,'IAPETUS'
;	oplot,r1tab/1d6,replicate(dat.titan.a,nr)/1d6,lin=2
;	xyouts,min(r1tab/1d6)*1.01,dat.titan.a*1.01/1d6,'TITAN'
;	rh_saturn=dat.saturn.a*(dat.saturn.m/dat.sun.m)^(1d0/3d0)
;	oplot,r1tab/1d6,replicate(rh_saturn,nr)/1d6,lin=2
;	xyouts,min(r1tab/1d6)*1.01,rh_saturn*1.01/1d6,'SATURN HILL RADIUS'

	plot,r1tab/1d6,ptab/!dpi,psym=-4
	oplot,r1tab/1d6,ptab/!dpi,psym=-4,col=2
	oplot,r1tab/1d6,etab,psym=-5,col=4
	xyouts,100,0.4,/data,'e'
	xyouts,100,1.8,/data,'pomega/PI'

	
	vinf2=+2d0*saturn_gm/r1tab
	v2=-saturn_gm/atab+vinf2
	vv=sqrt(v2)
	vinf=sqrt(vinf2)

	plot,r1tab/1d6,vinf/1d3,psym=-4, $
		yra=[0,max(vinf/1d3)]
	oplot,r1tab/1d6,vinf/1d3,psym=-4,col=2
	oplot,r1tab/1d6,vv/1d3,psym=-5,col=4

	delta_v=saturn_gm/2d0/atab/atab*100d3/vv
	;plot,r1tab/1d6,delta_v,/nod,title='DELTA v [m/s]'
	oplot,r1tab/1d6,delta_v,psym=-6,col=3

	delta_a=100d3
	tt=2d0/3d0*35d0*atab/delta_a

	oplot,r1tab/1d6,tt/3600d0,psym=-1,col=8
	xyouts,111,29,/data,'v_inf [km/s]',ori=-9
	xyouts,82,21,/data,'v_post [km/s]',ori=-12
	xyouts,82,10,/data,'t_inter [hours]'
	xyouts,119,1,/data,'dv [m/s]'

	rho=1000d0
	sigma=500d0	
	print,3d0/4d0*sigma/rho/(vinf/vv-1d0)

;
; testing consistency
;
	print
	print,'CHECK EQUATION 1 (LHS-RHS):'
	print,etab*cos(ptab)-(1d0-r1tab/r2)/(1d0+r1tab/r2)
	print,'CHECK EQUATION 2 (LHS-RHS):'
	print,atab*(1d0-etab*etab)-2d0*r1tab*r2/(r1tab+r2)

	f1=-pomega
	f2=-pomega+!dpi
	E1tab=dblarr(nr)	
	E2tab=dblarr(nr)	

	for ii=0,nr-1 do begin
		E1tab(ii)=get_E_Anomaly(atab(ii),etab(ii),r1tab(ii),f1)
		E2tab(ii)=get_E_Anomaly(atab(ii),etab(ii),r2,f2)
	endfor
	nn=sqrt(saturn_gm/atab/atab/atab)
	dt=35d0
	da=100d3
	tt=2d0/3d0*atab/da*dt
	print,'CHECK EQUATION 3 (LHS-RHS):'
	print,nn*tt-(E2tab-E1tab)+etab*(sin(E2tab)-sin(E1tab))
;plot_stamp,'GetElementsOfSwarm'
stop
end

;----------------------------------------------------
;------- MOMENTUM AND ENERGY BALANCE ----------------
;----------------------------------------------------
function bprime,bb,alpha
;
; angle of SCATTERED material (swarm) with kepler direction after
; 1st impact
;
	sqrt2=sqrt(2d0)

;	c1=1d0/sqrt2/alpha
;	c2=(sqrt2*cos(bb)-1d0)/sqrt2/sin(bb)

;	apu=c1*c2/(1d0+c2*c2)
;	sinbprime=-apu+sqrt((1d0-c1*c1)/(1d0+c2*c2)+apu*apu)

	mu=(sqrt2*cos(bb)-1d0)/sin(bb)
	mu2=mu*mu
	a2=alpha*alpha
	apu=sqrt2/alpha/(mu2+2d0)
	cosbprime=apu+sqrt((mu2-1d0/a2)/(mu2+2d0)+apu*apu)

	ind=where(cosbprime eq min(cosbprime))
	sign=replicate(1d0,n_elements(bb))
	sign(ind:*)=-1
	cosbprime=apu+sign*sqrt((mu2-1d0/a2)/(mu2+2d0)+apu*apu)
	
return,acos(cosbprime)
end

;----------------------------------------------------
function newton_r,xx

	common newton_r_par,rrprime,rmin,rmax,qq

	rr=xx
	rr2=rr*rr
	;rr3=rr2*rr
;
;	lhs=rrprime*(rr2+(rmax*rmax*rmax-rmin*rmin*rmin)/3d0/(rmax-rmin)+rr*(rmax+rmin))
;	rhs=rr3
	apu4=4d0-qq
	apu5=5d0-qq
	apu6=6d0-qq

	lhs=rrprime*(1d0 + $
		2d0/rr*apu4/apu5*(rmax^apu5-rmin^apu5)/(rmax^apu4-rmin^apu4) + $
		1d0/rr2*apu4/apu6*(rmax^apu6-rmin^apu6)/(rmax^apu4-rmin^apu4))
	rhs=rr

;return,rhs-lhs
return,rhs/lhs-1d0
end

;----------------------------------------------------
function marks,tab,step

	ind=indgen(n_elements(tab)/step)*step

return,tab(ind)
end

;----------------------------------------------------
pro RadiusConstraint,alpha0=alpha0

	common newton_r_par,rrprime,rmin,rmax,qq

	tek_color
	!p.charsize=2.3

	thick=2
	if !d.name eq 'PS' then begin
		thick=4
	endif else begin
		device,decomposed=0,retain=2
	endelse

	!x.thick=thick	
	!y.thick=thick	
	!p.thick=thick	


	rmin=1d-2
	rmax=1d2*rmin
	qq=3.1d0

	; bulk density of the projectile
	rho=1000d0
	; surface mass density of the ring
	sigma=500d0

	; characteristic length scale
	LL=3d0*sigma/4d0/rho

	; ratio between post-collisional to pre-collisional
	; speed 
	alpha=0.8d0
	if keyword_set(alpha0) then alpha=alpha0

	; f is the fraction of ring material that is crashed,
	; but NOT accelerated by the impact
	; (1-f) is the fraction of ring material that is dragged
	; along with the projectile after the impact
	ftab=[0.1d0,0.5d0,0.9d0]	
	nf=n_elements(ftab)

	; impact angle, measured between local kepler velocity and 
	; impact direction
	nb=150
	tiny=1d-2	
	btab=gentab(nb,0d0+tiny,!dpi-tiny)

	; table of (scaled) projectile radii
	rtab=dblarr(nb)

	; angle between the velocity of the SCATTERED projectile,
	; leaving the ring, and the local kepler velocity
	; this is generally smaller than btab
	bptab=bprime(btab,alpha)
	
	!p.multi=[0,1,3]
	pos=my_tile_to_Position(1,3,intery=1d-6)
	step=10
	
	window,/free
	!x.title='B [DEG]'
	plot,btab*180d0/!dpi,bptab*180d0/!dpi,psym=psym,xra=[0,180],xst=1, $
		yra=[0,180],yst=1,ytitle="B!U|!N [DEG]",pos=pos(0,*),xtitle='', $
		title='al='+strim(alpha,f='f3.1')+', sig='+ $
		strim(sigma,f='i4')+'kg/m**2, rho='+strim(rho,f='i4')+'kg/m**3, ' + $
		'rmax='+strim(rmax,f='f4.1')+'m, rmin='+strim(rmin,f='f4.2')+ $
		'm, q='+strim(qq,f='f3.1')
	
	oplot,[0,180],[0,180],lin=2

	rtab=alpha*sin(bptab)/(sin(btab)-alpha*sin(bptab))

	; non-scaled projectile radius
	rr=rtab*(1d0-ftab(0))*ll
	rr=dblarr(nf,nb)
	tol=1d-2
	rstart=10d0
	for iif=0,nf-1 do begin
	for ib=0,nb-1 do begin
		rrprime=rtab(ib)*(1d0-ftab(iif))*ll/sin(btab(ib))
		rr(iif,ib)=newton(rstart,'newton_r',/double,tolx=tol, $
			tolf=tol,itmax=500000L,tolmin=tol)
	;	rstart=rr(iif,ib)
	endfor	
;	print,newton_r(rstart)
	endfor
	plot,btab*180d0/!dpi,rr(0,*),psym=psym,xra=[0,180],xst=1, $
		ytitle='R_projectile [m]',pos=pos(1,*),xtitle='',ylog=1, $
		yra=[0.1,100d0],yst=1
	for iif=0,nf-1 do begin
		oplot,btab*180d0/!dpi,rr(iif,*),psym=psym,col=iif+2
		oplot,marks(btab*180d0/!dpi,step),marks(rr(iif,*),step),psym=4+iif,col=iif+2
	endfor

	
	Erat=dblarr(nb,nf)
	for iif=0,nf-1 do begin

		ff=ftab(iif)
		Erat(*,iif)=(ff+2d0*(1d0+rtab)*(1d0-ff)*alpha*alpha)/(1d0+2d0*rtab*(1d0-ff))

	endfor
	plot,btab*180d0/!dpi,erat(*,0),psym=psym,xra=[0,180],xst=1,ytitle='E_out/E_in', $
		yra=[min(erat),1d0],yst=1,pos=pos(2,*)
	for iif=0,nf-1 do begin
	oplot,btab*180d0/!dpi,erat(*,iif),psym=psym,col=iif+2
	oplot,marks(btab*180d0/!dpi,step),marks(erat(*,iif),step),psym=4+iif,col=iif+2
	endfor
	oplot,[0,180],[1,1],lin=2

	anno='f='+strim(ftab,f='f3.1')
	col=[2,3,4]
	psym=2+[2,3,4]
	lin=[0,0,0]

	dx=0.02
	dy=0.03
	xp=[0.13,0.21]
	yp=[1,1]*0.2
	plotanno,xp,yp,dx,dy,anno,lin,col,norm=1,psym=-psym,size=!p.charsize*0.8	

;plot_stamp,'GetElementsOfSwarm::RadiusConstraint'
end
