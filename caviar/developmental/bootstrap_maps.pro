;; ------------------- bootstrap_maps -----------------;;
;; LAST MODIFIED:  P.TORREY 5/15/08
;; DEPENDENCIES:   rev007movie.sav, rev007AZSCN.sav, etc.
;; OUTPUT:         ---
;; 
;; SUMMARY:	This program is somewhat of a mess, use with caution.
;;		This program was created to load in the edge profiles from the .sav
;;		file created in the bootstrapping routine, and then create visual
;;		graphics from these edge profiles.  The user can interactively
;;		specify the longitude range of interest, the profiles of interest,
;;		and create post_script files from the displayed graphics.  This program
;;		works great if executed in the directory containing the appropriate
;;		save files, and will fail otherwise.  This program is not generalized
;;		but the ideas here are useful for displaying multiple edge profiles.


include007movie=1
include008movie=1
includeAZSCN=1
includeResonance = 1

start:

PDF = 0
window, xs=1800, ys=1050

longmax = 0.0
longmin = 0.0
read,longmin,prompt='Minimum Longitude :  '
read,longmax,prompt='Maximum Longitude :  '

if (longmin lt -180.0) then longmin = -180.0
if (longmax gt 180.0)  then longmax =  180.0

; -------- LOAD PREDICTED LOBE LOCATIONS ------- ;

if keyword_set(findfile('lobeLocations007.sav')) then begin
	restore,'lobeLocations007.sav'
	movie007 = bootstrap_lobe_long(1:(size(bootstrap_lobe_long))(1)-1)
	movie007mod = movie007;-1.625;	
	image007 = images(1:(size(movie007))(1)-1)		;; 1.625
endif

if keyword_set(findfile('lobeLocations008.sav')) then begin
	restore,'lobeLocations008.sav'
	movie008 = bootstrap_lobe_long(1:(size(bootstrap_lobe_long))(1)-1)
	movie008mod = movie008; - 8.039;
	image008 = images(1:(size(movie008))(1)-1)		;; 8.039
endif

if keyword_set(findfile('lobeLocationsAZSCN.sav')) then begin
	restore,'lobeLocationsAZSCN.sav'
	AZSCN007 = bootstrap_lobe_long				;; 4.18
	AZSCN007mod = AZSCN007; - 4.18;
endif

; ----------- LOAD EDGE PROFILES --------------- ;

if keyword_set(findfile('rev007movie.sav')) then begin
	rev007movie = 1
	restore,'rev007movie.sav'

	redgearr1 = bigredge1
	redgearr0 = bigredge2

grapharr1 = bootstrap_maps_trim(redgearr1,173,180)

	restore,'rev007movie.sav'

	redgearr1 = bigredge1
	redgearr0 = bigredge2

longmaxTEMP = longmax
if (longmax gt 147.2) then longmaxTEMP = 147.2

	redgearr0 = bootstrap_maps_trim(redgearr0,longmin,longmaxTEMP)
	redgearr1 = bootstrap_maps_trim(redgearr1,longmin,longmaxTEMP)

	redgearr0(1,*) = redgearr0(1,*)+1.5-136522
	redgearr1(1,*) = redgearr1(1,*)-136485+15
	grapharr1(1,*) = grapharr1(1,*)-136485+15
endif

if keyword_set(findfile('rev008movie.sav')) then begin
	rev008movie = 1
	restore,'rev008movie.sav'

	redgearr3 = bigredge1
	redgearr2 = bigredge2

	redgearr2(1,*) = redgearr2(1,*)-4.5-136522
	redgearr3(1,*) = redgearr3(1,*)-136485-13

	badpoints = where(redgearr2(1,*) gt 10-5.5)
	removedcount = 0
	for i=0,(size(badpoints))(1)-1 do begin
		redgearr2 = [[redgearr2(*,0:badpoints(i)-1-removedcount)],[redgearr2(*,badpoints(i)+1-removedcount:(size(redgearr2))(2)-1)]]
		removedcount = removedcount+1
	endfor 

	redgearr2 = bootstrap_maps_trim(redgearr2,longmin,longmax)
	redgearr3 = bootstrap_maps_trim(redgearr3,longmin,longmax)
endif



if keyword_set(findfile('Keeler_0_50.sav')) then begin
	rev007AZSCN = 1
	restore,'Keeler_0_50.sav'

	redgearr5 = [ [ keelerinnerx] , [keelerinnery] ]					;; 4/19
	redgearr4 = [ [ keelerouterx] , [keeleroutery] ]					;; 4/19
	redgearr4 = transpose(redgearr4)
	redgearr5 = transpose(redgearr5)

	restore,'Keeler_51_100.sav'
	
	redgearr7 =  [ [keelerinnerx],[keelerinnery]]
	redgearr6 =  [ [keelerouterx],[keeleroutery]]
	redgearr6 = transpose(redgearr6)
	redgearr7 = transpose(redgearr7)

	restore,'Keeler_101_150.sav'

	redgearr9 = [ [keelerinnerx],[keelerinnery]]
	redgearr8 = [ [keelerouterx],[keeleroutery]]
	redgearr8 = transpose(redgearr8)
	redgearr9 = transpose(redgearr9)

	restore,'Keeler_151_200.sav' ; This sequence contains Daphnus

	redgearr11  = [ [keelerinnerx],[keelerinnery] ]
	redgearr10  = [ [keelerouterx],[keeleroutery] ]
	redgearr10 = transpose(redgearr10)
	redgearr11 = transpose(redgearr11)

	restore,'Keeler_201_230.sav'

	redgearr13 = [ [keelerinnerx],[keelerinnery] ]
	redgearr12 = [ [keelerouterx],[keeleroutery] ]
	redgearr12 = transpose(redgearr12)
	redgearr13 = transpose(redgearr13)

	redgearr4 = [ [redgearr4],[redgearr6],[redgearr8],[redgearr10],[redgearr12] ]
	redgearr5 = [ [redgearr5],[redgearr7],[redgearr9],[redgearr11],[redgearr13] ]

	redgearr4(0,*) = redgearr4(0,*)-7.589+0.035
	redgearr5(0,*) = redgearr5(0,*)-7.589-3.5-0.035
	redgearr4(1,*) = redgearr4(1,*)-136522
	redgearr5(1,*) = redgearr5(1,*)-136485

	redgearr4 = redgearr4[*,sort(redgearr4(0,*))]
	redgearr5 = redgearr5[*,sort(redgearr5(0,*))]

	badpoints = where(redgearr4(1,*) gt 5)
	removedcount = 0
	for i=0,(size(badpoints))(1)-1 do begin
		redgearr4 = [[redgearr4(*,0:badpoints(i)-1-removedcount)],[redgearr4(*,badpoints(i)+1-removedcount:(size(redgearr4))(2)-1)]]
		removedcount = removedcount+1
	endfor 

	badpoints = where(redgearr5(1,*) lt 136476.5-136485)   ;; change this back to 136475
	removedcount = 0
	for i=0,(size(badpoints))(1)-1 do begin
;		if badpoints(i) eq 0
		redgearr5 = [[redgearr5(*,0:badpoints(i)-1-removedcount)],[redgearr5(*,badpoints(i)+1-removedcount:(size(redgearr5))(2)-1)]]
		removedcount = removedcount+1
	endfor 


	redgearr4 = bootstrap_maps_trim(redgearr4,longmin,longmax)
	redgearr5 = bootstrap_maps_trim(redgearr5,longmin,longmax)
endif

if keyword_set(findfile('Encke035.sav')) then begin
	rev035AZSCN = 1
	restore,'Encke035.sav'
	
	redgearr7 = [[enckeinnerx-31.75],[enckeinnery]]
	redgearr6 = [[enckeouterx-31.75],[enckeoutery]]
	redgearr6 = transpose(redgearr6)
	redgearr7 = transpose(redgearr7)

	badpoints = where(redgearr7(1,*) lt -5)
	removedcount = 0
	for i=0,(size(badpoints))(1)-1 do begin
		redgearr7 = [[redgearr7(*,0:badpoints(i)-1-removedcount)],[redgearr7(*,badpoints(i)+1-removedcount:(size(redgearr7))(2)-1)]]
		removedcount = removedcount+1
	endfor 

	redgearr6 = bootstrap_maps_trim(redgearr6,longmin,longmax)
	redgearr7 = bootstrap_maps_trim(redgearr7,longmin,longmax)
endif

if ( keyword_set(redgearr0) || keyword_set(redgearr2) || keyword_set(redgearr4) || keyword_set(redgearr6) )then begin
	print,'Plotting over Selected Range ...'
endif else begin
	print,'Invalid longitude Range Selected'
	stop
endelse

lmin = longmin
lmax = longmax

if keyword_set(redgearr0) then begin
	rmin0= min(redgearr0(1,*))
	rmax0= max(redgearr0(1,*))
	rmin1= min(redgearr1(1,*))	; For the rev 007 Movie
	rmax1= max(redgearr1(1,*))
endif

if keyword_set(redgearr2) then begin
	rmin2= min(redgearr2(1,*))
	rmax2= max(redgearr2(1,*))	; For the rev 008 Movie
	rmin3= min(redgearr3(1,*))
	rmax3= max(redgearr3(1,*))
endif

if keyword_set(redgearr4) then begin
	rmin4= min(redgearr4(1,*))
	rmax4= max(redgearr4(1,*))	; For the rev 007 AZSCAN
	rmin5= min(redgearr5(1,*))
	rmax5= max(redgearr5(1,*))
endif

if keyword_set(redgearr6) then begin
	rmin6= min(redgearr6(1,*))
	rmax6= max(redgearr6(1,*))
	rmin7= min(redgearr7(1,*))
	rmax7= max(redgearr7(1,*))
endif

if keyword_set(rev007movie) then begin
	rminTOP = min( [ rmin0, rmin2, rmin4 ] )
	rminBOT = min( [ rmin1, rmin3, rmin5 ] )
	rmaxTOP = max( [ rmax0, rmax2, rmax4 ] )
	rmaxBOT = max( [ rmax1, rmax3, rmax5 ] ) 
	STDmax = stddev( [[redgearr0(1,*)],[redgearr1(1,*)],[redgearr2(1,*)],[redgearr3(1,*)],[redgearr4(1,*)],[redgearr5(1,*)]   ] )
endif

plotPDF:

clr = [ctred(),ctwhite(),ctgreen() ]

xr = [lmin,lmax] + (lmax-lmin) * [-.1,.1]

!p.charsize=2.0
!p.multi = [0,1,2]
!x.margin = [13,3]
!y.margin = [0,5]
!y.omargin = [4,3]

bar = findgen(100)/100.0

replot:
if keyword_set(PDF) then begin
xr = [lmin,lmax] + (lmax-lmin) * [-.02,.02]
if keyword_set(rev007movie) then begin
	!p.multi = 0
	!x.margin = [6,0]
	!y.margin = 0
	lzr,'KeelerOuter',aspect=9.0/16.0
	@plot_prepare
	plot_color
	clr = [ctred(),ctwhite(),ctgreen() ]
	yr = [rminTOP,rmaxTOP] + (rmaxTOP-rminTOP)*[-.1,.1]

	xtn = ''
	xtit = 'Co-Rotating Longitude (!Uo!N) Relative to Daphnis'

	plot_nosci,xr,yr,/nodata,/xs,/ys,xtit=xtit,ytit='Radial Variations (km)',xtickn=xtn;,title='Keeler Gap Outer Edge'
	if keyword_set(include007movie) then oplot,redgearr0[0,*],redgearr0[1,*],color=clr(0)
	if keyword_set(include008movie) then oplot,redgearr2[0,*],redgearr2[1,*];,color=clr(1)
	if keyword_set(includeAZSCN) then oplot,redgearr4[0,*],redgearr4[1,*],color=clr(2)

	clzr

	lzr,'KeelerInner',aspect=9.0/16.0
	@plot_prepare
	plot_color
	clr = [ctred(),ctwhite(),ctgreen() ]
	yr = [rminBOT,rmaxBOT] + (rmaxBOT-rminBOT)*[-.1,.1]
	bar = bar*(yr[1] - yr[0]) + yr[0]
	plot_nosci,xr,yr,/nodata,/xs,/ys,xtit=xtit,ytit='RadialVariations (km)',xtickn=xtn,title='Keeler Gap Inner Edge',xticki=30
	if keyword_set(include007movie) then oplot,redgearr1[0,*],redgearr1[1,*],color=clr(0),thick=2
	if keyword_set(grapharr1) then oplot,grapharr1[0,*],grapharr1[1,*],color=clr(0),thick=2
	if keyword_set(include008movie) then oplot,redgearr3[0,*],redgearr3[1,*];,color=clr(1),thick=2
	if keyword_set(includeAZSCN) then oplot,redgearr5[0,*],redgearr5[1,*],color=clr(2),thick=2

	if keyword_set(includeResonance) then begin
		if keyword_set(include007movie) then begin
			for i=0,(size(movie007))(1)-1 do oplot,bar*0+movie007(i),bar,color=clr(0);
		endif
		if keyword_set(include008movie) then begin
			for i=0,(size(movie008))(1)-1 do oplot,bar*0+movie008(i),bar;,color=clr(1);
		endif	
		if keyword_set(includeAZSCN) then begin
			for i=0,(size(AZSCN007))(1)-1 do oplot,bar*0+AZSCN007(i),bar,color=clr(2)
		endif
	endif

	clzr
endif
if keyword_set(rev035AZSCN) then begin
	!p.multi = 0
	!x.margin = [6,0]
	!y.margin = 0

	lzr,'EnckeOuter',aspect = 9.0/16.0
	@plot_prepare
	plot_color
	clr = [ctred(),ctwhite(),ctgreen() ]
	yr = [rmin6,rmax6] + (rmax6-rmin6)*[-.1,.1]

	xtn = ''
	xtit = 'Co-Rotating Longitude (!Uo!N) Relative to Pan'

	plot_nosci,xr,yr,/nodata,/xs,/ys,xtit=xtit,ytit='Radial Variations (km)',xtickn=xtn,title='Encke Gap Outer Edge'
	if keyword_set(includeAZSCN) then oplot,redgearr6[0,*],redgearr6[1,*];,color=clr(2)

	clzr

	lzr,'EnckeInner',aspect=9.0/16.0
	@plot_prepare
	plot_color
	clr = [ctred(),ctwhite(),ctgreen() ]

	yr = [rmin7,rmax7] + (rmax7-rmin7)*[-.1,.1]
	plot_nosci,xr,yr,/nodata,/xs,/ys,xtit=xtit,ytit='Radial Variations (km)',xtickn=xtn,title='Encke Gap Inner Edge',xticki=30
	if keyword_set(includeAZSCN) then oplot,redgearr7[0,*],redgearr7[1,*];,color=clr(2)

	
	clzr
endif
endif else begin
if keyword_set(rev007movie) then begin
	yr = [rminTOP,rmaxTOP] + (rmaxTOP-rminTOP)*[-.1,.1]
	ytit = ['Keeler Gap Outer Edge','Keeler Gap Inner Edge']


	xtn = ''
	xtit = 'Co-Rotating Longitude (!Uo!N) Relative to Daphnis'

	plot_nosci,xr,yr,/nodata,/xs,/ys,xtit=xtit,ytit=ytit[0],xtickn=xtn;,title='Keeler Gap Outer Edge'
	if keyword_set(include007movie) then oplot,redgearr0[0,*],redgearr0[1,*],color=clr(0)
	if keyword_set(include008movie) then oplot,redgearr2[0,*],redgearr2[1,*],color=clr(1)
	if keyword_set(includeAZSCN) then oplot,redgearr4[0,*],redgearr4[1,*],color=clr(2)

	xtn = ''
	xtit = 'Co-Rotating Longitude (!Uo!N) Relative to Daphnis'

	yr = [rminBOT,rmaxBOT] + (rmaxBOT-rminBOT)*[-.1,.1]
	bar = bar*(yr[1] - yr[0]) + yr[0]
	plot_nosci,xr,yr,/nodata,/xs,/ys,xtit=xtit,ytit=ytit[1],xtickn=xtn,xticki=30
	if keyword_set(include007movie) then oplot,redgearr1[0,*],redgearr1[1,*],color=clr(0)
	if keyword_set(grapharr1) then oplot,grapharr1[0,*],grapharr1[1,*],color=clr(0)
	if keyword_set(include008movie) then oplot,redgearr3[0,*],redgearr3[1,*],color=clr(1)
	if keyword_set(includeAZSCN) then oplot,redgearr5[0,*],redgearr5[1,*],color=clr(2)

	if keyword_set(includeResonance) then begin
		if keyword_set(include007movie) then begin
			for i=0,(size(movie007))(1)-1 do oplot,bar*0+movie007mod(i),bar,color=clr(0);
		endif
		if keyword_set(include008movie) then begin
			for i=0,(size(movie008))(1)-1 do oplot,bar*0+movie008mod(i),bar,color=clr(1);
		endif	
		if keyword_set(includeAZSCN) then begin
			for i=0,(size(AZSCN007))(1)-1 do oplot,bar*0+AZSCN007mod(i),bar,color=clr(2)
		endif
	endif
endif
if keyword_set(rev035AZSCN) then begin
	yr = [rmin6,rmax6] + (rmax6-rmin6)*[-.1,.1]

	xtn = ''
	xtit = 'Co-Rotating Longitude (!Uo!N) Relative to Pan'

	plot_nosci,xr,yr,/nodata,/xs,/ys,xtit=xtit,ytit='Radial Variations (km)',xtickn=xtn,title='Encke Gap Outer Edge'
	if keyword_set(includeAZSCN) then oplot,redgearr6[0,*],redgearr6[1,*],color=clr(2)

	yr = [rmin7,rmax7] + (rmax7-rmin7)*[-.1,.1]
	plot_nosci,xr,yr,/nodata,/xs,/ys,xtit=xtit,ytit='Radial Variations (km)',xtickn=xtn
	if keyword_set(includeAZSCN) then oplot,redgearr7[0,*],redgearr7[1,*],color=clr(2)
endif
endelse

menu:

PDF = 0

reply = ''
while reply eq '' do begin
	print, 'Select New Range [r], PDF [p], filled map [f], or quit [q]?'
	print, 'Toggle 007 movie[7], Toggle 008 movie[8], Toggle AZSCN [AZ]?'
	read,reply 
	case reply of
		'r': goto, start
		'p': begin
			PDF = 1
			goto, plotPDF
		end
		'q': goto, quitting
		'7': begin
			if (include007movie eq 0) then begin
 				include007movie = 1
			endif else begin
				include007movie = 0
			endelse
			goto, replot			
		end
		'8': begin
			if (include008movie eq 0) then begin
				include008movie = 1
			endif else begin
				include008movie = 0
			endelse
			goto, replot			
		end
		'AZ': begin
			if (includeAZSCN eq 0) then begin
				includeAZSCN = 1
			endif else begin
				includeAZSCN = 0
			endelse
			goto, replot			
		end
		'l': begin
			if (includeResonance eq 0) then begin
				includeResonance = 1
			endif else begin
				includeResonance = 0
			endelse
			goto, replot
		end
		'f': goto, full_gap_plot
		'fp': begin
			PDF = 1	
			goto, full_gap_plot
		end
		else: reply = ''
	endcase
endwhile

full_gap_plot:


if keyword_set(PDF) then begin
	lzr,'GapProfile'
	@plot_prepare
	plot_color
	clr = [ctred(),ctwhite(),ctgreen() ]
endif
!p.multi = 0
!p.charsize = 1.0
!x.margin = [2,0]
!y.margin = [18,18]

xtn = ''
xtit = 'Co-Rotating Longitude (!Uo!N)'
xr = [lmin,lmax]

if keyword_set(rev007movie) then begin



	redgearr0(1,*) = redgearr0(1,*) -1.5+136522
	redgearr1(1,*) = redgearr1(1,*) -10+ 136485
	mean0 = median(redgearr0(1,*))
	print,mean0
	mean1 = median(redgearr1(1,*))
	print,mean1
	Daphnisy = mean([mean0,mean1])
	print,Daphnisy

	ychange = 136505-Daphnisy
	print,ychange

	yr = [min(redgearr1(1,*)),max(redgearr0(1,*))] + (max(redgearr0(1,*))-min(redgearr1(1,*)))*[-.1,.1]
	plot_nosci,xr,yr+ychange,/nodata,/xs,/ys,xtit=xtit,ytit='Radial Scale (km)',xtickn=xtn,xticki=30;,title='KeelerGap'
	polyfill,[ [min(redgearr0(0,*))], [redgearr0(0,*)], [max(redgearr0(0,*))] ],[ [yr[1]], [redgearr0(1,*)],[yr[1]] ]+ychange
	polyfill,[ [min(redgearr1(0,*))], [redgearr1(0,*)], [max(redgearr1(0,*))] ],[ [yr[0]], [redgearr1(1,*)], [yr[0]] ]+ychange
	oplot,redgearr0(0,*),redgearr0(1,*)+ychange,thick=2
	oplot,redgearr1(0,*),redgearr1(1,*)+ychange,thick=4

	sizey = (yr[1]-yr[0])/80
	sizex = (xr[1]-xr[0])/600
	tempRad = findgen(1000)/1000*2*3.14159
	tempx = sizex*cos(tempRad)
	tempy = sizey*sin(tempRad)+136505

	polyfill,tempx,tempy

	redgearr0(1,*) = redgearr0(1,*) + 1.5-136522
	redgearr1(1,*) = redgearr1(1,*) - 10 -136485
endif
if keyword_set(rev035AZSCN) then begin
	yr = [rmin7,rmax6] + (rmax6 - rmin7)*[-.1,.1]
	plot_nosci,xr,yr,/nodata,/xs,/ys,xtit=xtit,ytit='Radial Scale (km)',xtickn=xtn,title='Encke Gap'
	polyfill,[ [min(redgearr6(0,*))], [redgearr6(0,*)], [max(redgearr6(0,*))] ],[ [yr[1]], [redgearr6(1,*)], [yr[1]] ]
	polyfill,[ [min(redgearr7(0,*))], [redgearr7(0,*)], [max(redgearr7(0,*))] ],[ [yr[0]], [redgearr7(1,*)], [yr[0]] ]	
endif

if keyword_set(PDF) then clzr

PDF = 0

goto, menu


quitting:
end

