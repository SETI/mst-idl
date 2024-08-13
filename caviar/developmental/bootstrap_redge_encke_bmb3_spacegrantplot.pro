;;This version of bootstrap has different colors for the first few
;;images. It was modified by M. Tiscareno to make plots in which edge
;;profiles from different images were easier to see. Besides that
;;purpose there is no reason to use this code over bootstrap_redge_encke_bmb3.pro
;; -------------------- bootstrap_redge_encke_bmb3_spacegrantplot.pro ------------------ ;;
;; LAST MODIFIED:  P.TORREY 5/15/08. B. BYINGTON 7/09.
;; DEPENDENCIES:   .edge files for images in sequence
;; 
;; SUMMARY:	   This the the MAIN code.  It takes the individual edge profiles
;;		   which must be stored in the format:
;;			image_name.linsamEI  (for Encke Inner)
;;			image_name.linsamEO  (for Encke Outer)
;;			image_name.linsamKI  (for Keeler Inner)
;;			image_name.linsamKO  (for Keeler Outer)
;;			image_name.linsamR   (for outer edge of the A ring/Roche division)
;;		   This code will load these edge profiles, and allow the user to move them according
;;		   in the longitudinal, radial, and time directions.  Additional edge profiles can
;;		   be added through the interactive menu, allowing for global maps to be created.
;;		   SOME MODIFICATION OF THIS CODE MAY BE REQUIRED TO EITHER ENABLE/DISABLE ASSUMED
;;		   OR FIXED PATTERN SPEEDS.  The "dt" variable can either be fixed for every image, 
;;		   or it can be a free parameter.


firstimage = 1000
lastimage  = 0
rind = 10
imagenum = 0

redgearr0=0
redgearr1=0
redgearr2=0
redgearr3=0
redgearr4=0
redgearr5=0
redgearr6=0
redgearr7=0
redgearr8=0
redgearr9=0

EIcount=0
EOcount=0
KIcount=0
KOcount=0
Rcount=0

EInew=1
EOnew=1
KInew=1
KOnew=1
Rnew=1

dr=0
dl=0
dtwist=0
dt1=0
dt3=0
dt5=0
dt7=0
dt9=0
_dr=0
_dl=0
_dtwist=0
_dt1=0
_dt3=0
_dt5=0
_dt7=0
_dt9=0

@bootstrap_initialize
help,polera,poledec

device, retain=2

;-------------------------------------------------------------------------------;

add:

if (EIcount ge 2) then begin
  if keyword_set(EnckeInner) then   redgearr1(rind,*) = redgearr1(rind,*)+dt1
  if keyword_set(EnckeInner) then   redgearr0 = [[redgearr0],[redgearr1]]
  if keyword_set(EnckeInner) then   redgearr0 = redgearr0[*,sort(redgearr0(rind,*))]
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, dt9,  filename=imageNEW+'_bsa'
endif

if (EOcount ge 2) then begin
  if keyword_set(EnckeOuter) then  redgearr3(rind,*) = redgearr3(rind,*)+dt3
  if keyword_set(EnckeOuter) then  redgearr2 = [[redgearr2],[redgearr3]]
  if keyword_set(EnckeOuter) then  redgearr2 = redgearr2[*,sort(redgearr2(rind,*))]
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, dt9,  filename=imageNEW+'_bsa'
endif

if (KIcount ge 2) then begin
  if keyword_set(KeelerInner) then  redgearr5(rind,*) = redgearr5(rind,*)+dt5
  if keyword_set(KeelerInner) then  redgearr4 = [[redgearr4],[redgearr5]]
  if keyword_set(KeelerInner) then  redgearr4 = redgearr4[*,sort(redgearr4(rind,*))]
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, dt9,  filename=imageNEW+'_bsa'
endif

if (KOcount ge 2) then begin
  if keyword_set(KeelerOuter) then redgearr7(rind,*) = redgearr7(rind,*)+dt7
  if keyword_set(KeelerOuter) then redgearr6 = [[redgearr6],[redgearr7]]
  if keyword_set(KeelerOuter) then redgearr6 = redgearr6[*,sort(redgearr6(rind,*))]
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, dt9, filename=imageNEW+'_bsa'
endif

if (Rcount ge 2) then begin
  if keyword_set(Roche)   then     redgearr9(rind,*) = redgearr9(rind,*)+dt9
  if keyword_set(Roche)    then    redgearr8 = [[redgearr8],[redgearr9]]
  if keyword_set(Roche)   then     redgearr8 = redgearr8[*,sort(redgearr8(rind,*))]
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, dt9, filename=imageNEW+'_bsa'
endif


if (EIcount eq 1) then begin
  if keyword_set(EnckeInner) then   redgearr0 = redgearr1
  if keyword_set(EnckeInner) then   redgearr0(rind,*) = redgearr0(rind,*)+dt1
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, dt9, filename=imageNEW+'_bsa'
endif 

if (EOcount eq 1) then begin
  if keyword_set(EnckeOuter) then  redgearr2 = redgearr3
  if keyword_set(EnckeOuter) then  redgearr2(rind,*) = redgearr2(rind,*)+dt3
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, dt9, filename=imageNEW+'_bsa'
endif

if (KIcount eq 1) then begin
  if keyword_set(KeelerInner) then  redgearr4 = redgearr5
  if keyword_set(KeelerInner) then  redgearr4(rind,*) = redgearr4(rind,*)+dt5
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, dt9, filename=imageNEW+'_bsa'
endif

if (KOcount eq 1) then begin
  if keyword_set(KeelerOuter) then redgearr6 = redgearr7
  if keyword_set(KeelerOuter) then redgearr6(rind,*) = redgearr6(rind,*)+dt7
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, dt9, filename=imageNEW+'_bsa'
endif

if (Rcount eq 1) then begin
  if keyword_set(Roche)   then     redgearr8 = redgearr9
  if keyword_set(Roche) then       redgearr8(rind,*) = redgearr8(rind,*)+dt9
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, dt9, filename=imageNEW+'_bsa'
endif

if keyword_set(EInew) and keyword_set(EOnew) and keyword_set(KInew) and keyword_set(KOnew) and keyword_set(Rnew) or keyword_set(chooseimage) then begin
  imageNEW=''
  read,imageNEW,prompt='NEW IMAGE NUMBER : '
  imageNEW = fix(imageNEW)
  imagenum = fix(imageNEW)
  if (imagenum ge lastimage)  then lastimage = imagenum
  if (imagenum le firstimage) then firstimage= imagenum
endif else begin
  ;;;if (imagenum eq 25) then imagenum = imagenum+2 ???
  imagenum = imagenum+1 ;;;going forwards (original code)
  ;imagenum = imagenum-1 ;;;going backwards
  if (imagenum ge lastimage)  then lastimage = imagenum
  if (imagenum le firstimage) then firstimage= imagenum
  imageNEW = imagenum
end  

print,'CURRENT IMAGE:'
print,imagenum

imageNEW = filenames(imageNEW)
periodNEW= rstrpos(imageNEW,'.')
stemNEW  = strmid(imageNEW,0,periodNEW)
files = file_search(stemNEW+'.edge_linsam*')

if( (size(files))(0) eq 0) then stop
nedges = (size(files))(1);
titles = strmid(files,29,2)
KeelerInner = max(titles eq 'KI')
KeelerOuter= max(titles eq 'KO')
EnckeInner  = max(titles eq 'EI')
EnckeOuter = max(titles eq 'EO')
Roche       = max(titles eq 'R')

_dr = 0
_dt1 = 0
_dt3 = 0
_dt5 = 0
_dt7 = 0
_dt9 = 0
_dl = 0
_dtwist = 0
dr=0
dl=0
dtwist=0
dt1=0
dt3=0
dt5=0
dt7=0
dt9=0

;------------------ Import New Edges ----------------------------;

if keyword_set(EnckeInner) then begin
	restore, stemNEW+'.edge_linsamEI'
	redge1 = linsam_to_redge_encke( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
	redgearr1 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	EIcount = EIcount+1
endif else begin
	redgearr1 = 0
endelse

if keyword_set(EnckeOuter) then begin
	restore, stemNEW+'.edge_linsamEO'
	redge1 = linsam_to_redge_encke( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
	redgearr3 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	EOcount = EOcount+1
endif else begin
	redgearr3 = 0
endelse

if keyword_set(KeelerInner) then begin
	restore, stemNEW+'.edge_linsamKI'
	redge1 = linsam_to_redge_encke( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
	redgearr5 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	KIcount = KIcount+1
endif else begin
	redgearr5=0
endelse

if keyword_set(KeelerOuter) then begin
	restore, stemNEW+'.edge_linsamKO'
	redge1 = linsam_to_redge_encke( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
	redgearr7 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	KOcount = KOcount+1
endif else begin
	redgearr7 = 0
endelse

if keyword_set(Roche) then begin
	restore, stemNEW+'.edge_linsamR'
	redge1 = linsam_to_redge_encke( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
	redgearr9 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	Rcount = Rcount+1
endif else begin
	redgearr9 = 0
endelse

cmat = REDGE_CMAT
cmat_orig = cmat

; -------------------- Filter New Edges -----------------------------;
filtersize= 2.5

if keyword_set(redgearr1) then begin
	redgearr1 = medianfilter(redgearr1,filtersize,rind)
endif

if keyword_set(redgearr3) then begin
	redgearr3 = medianfilter(redgearr3,filtersize,rind)
endif

if keyword_set(redgearr5) then begin
	redgearr5 = medianfilter(redgearr5,filtersize,rind)
endif

if keyword_set(redgearr7) then begin
	redgearr7 = medianfilter(redgearr7,filtersize,rind)
endif

if keyword_set(Roche) then begin
	redgearr9 = medianfilter(redgearr9,filtersize,rind)
endif


;--------------------- Adjust New Edges -----------------------------;

if keyword_set(findfile(imageNEW+'_bsa')) then begin
  restore, imageNEW+'_bsa'
  print,'_bsa file found and loading...'
  _dr = dr
  _dt1 = dt1
  _dt3 = dt3
  _dt5 = dt5
  _dt7 = dt7
  _dt9 = dt9
  _dl = dl
  _dtwist = dtwist
  goto, adjust
endif

nextplot:
if !d.name eq 'X' then window, xs=1200, ys=800  ;xs=1700, ys=1050    ; xs=1800, ys=1150

_dt1 = 0
_dt1 = dt1
_dt3 = 0
_dt3 = dt3
_dt5 = 0
_dt5 = dt5
_dt7 = 0
_dt7 = dt7
_dt9 = 0
_dt9 = dt9

;   This handles the graph parameters 
!p.charsize=1.5
!p.multi = [0,1,nedges] 
!x.margin = [13,3]
!y.margin = [0,0]
!y.omargin = [4,2]


clr = [ ctred(), ctwhite(), ctgreen() ]
clr1 = [ ctred(), ctgreen(), ctblue(), ctyellow(), ctpurple(), ctcyan() ]
clr1 = [ clr1, clr1 ]

;---------------- GRAPHING LOOP -------------------;

; X Limits for Inner and Outer Edge (constant for both)
xtn = replicate(' ',20) 
xtit = ' '
;xtit = 'Co-Rotating Longitude (!Uo!N)'
ytit = ['Encke Gap Inner Edge' ,'Encke Gap Outer Edge','Keeler Gap Inner Edge','Keeler Gap Outer Edge','Roche Gap Edge']

lmin = 1000
lmax = -1000

rmin01 = 1000000
rmax01 = 0
rmin23 = 1000000
rmax23 = 0
rmin45 = 1000000
rmax45 = 0
rmin67 = 1000000
rmax67 = 0
rmin89 = 1000000
rmax89 = 0

if keyword_set(redgearr0) then begin
	lmin = min( [lmin, min(redgearr0[rind,*]) ] )
	lmax = max( [lmax, max(redgearr0[rind,*]) ] )
	rmin01 = min( [rmin01, min(redgearr0[rind+1,*]) ] )
	rmax01 = max( [rmax01, max(redgearr0[rind+1,*]) ] )
endif

if keyword_set(redgearr1) then begin
	lmin = min( [lmin, min(redgearr1[rind,*]) +_dt1 ])
	lmax = max( [lmax, max(redgearr1[rind,*]) +_dt1 ])
	rmin01 = min( [rmin01, min(redgearr1[rind+1,*]) ] )
	rmax01 = max( [rmax01, max(redgearr1[rind+1,*]) ] )
endif

if keyword_set(redgearr2) then begin
	lmin = min( [ lmin, min(redgearr2[rind,*]) ])
	lmax = max( [ lmax, max(redgearr2[rind,*]) ])
	rmin23 = min( [rmin23, min(redgearr2[rind+1,*]) ] )
	rmax23 = max( [rmax23, max(redgearr2[rind+1,*]) ] )
endif

if keyword_set(redgearr3) then begin
	lmin = min( [ lmin, min(redgearr3[rind,*]) +_dt3 ])
	lmax = max( [ lmax, max(redgearr3[rind,*]) +_dt3 ])
	rmin23 = min( [rmin23, min(redgearr3[rind+1,*]) ] )
	rmax23 = max( [rmax23, max(redgearr3[rind+1,*]) ] )
endif

if keyword_set(redgearr4) then begin
	lmin = min( [ lmin, min(redgearr4[rind,*]) ] )
	lmax = max( [ lmax, max(redgearr4[rind,*]) ] )
	rmin45 = min( [rmin45, min(redgearr4[rind+1,*]) ] )
	rmax45 = max( [rmax45, max(redgearr4[rind+1,*]) ] )
endif

if keyword_set(redgearr5) then begin
	lmin = min( [ lmin, min(redgearr5[rind,*]) +_dt5 ])
	lmax = max( [ lmax, max(redgearr5[rind,*]) +_dt5 ])
	rmin45 = min( [rmin45, min(redgearr5[rind+1,*]) ] )
	rmax45 = max( [rmax45, max(redgearr5[rind+1,*]) ] )
endif

if keyword_set(redgearr6) then begin
	lmin = min( [ lmin, min(redgearr6[rind,*]) ])
	lmax = max( [ lmax, max(redgearr6[rind,*]) ])
	rmin67 = min( [rmin67, min(redgearr6[rind+1,*]) ] )
	rmax67 = max( [rmax67, max(redgearr6[rind+1,*]) ] )
endif

if keyword_set(redgearr7) then begin
	lmin = min( [ lmin, min(redgearr7[rind,*]) +_dt7 ])
	lmax = max( [ lmax, max(redgearr7[rind,*]) +_dt7 ])
	rmin67 = min( [rmin67, min(redgearr7[rind+1,*]) ] )
	rmax67 = max( [rmax67, max(redgearr7[rind+1,*]) ] )
endif

if keyword_set(redgearr8) then begin
	lmin = min( [ lmin, min(redgearr8[rind,*]) ] )
	lmax = max( [ lmax, max(redgearr8[rind,*]) ] )
	rmin89 = min( [rmin89, min(redgearr8[rind+1,*]) ] )
	rmax89 = max( [rmax89, max(redgearr8[rind+1,*]) ] )
endif

if keyword_set(redgearr9) then begin
	lmin = min( [ lmin, min(redgearr9[rind,*]) +_dt9 ])
	lmax = max( [ lmax, max(redgearr9[rind,*]) +_dt9 ])
	rmin89 = min( [rmin89, min(redgearr9[rind+1,*]) ] )
	rmax89 = max( [rmax89, max(redgearr9[rind+1,*]) ] )
endif


xr = [lmin,lmax] + (lmax-lmin)*[-.1,.1]

if keyword_set(redgearr8) || keyword_set(redgearr9) then begin
	yr = [rmin89,rmax89] + (rmax89-rmin89)*[-.1,.1]
	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[4], xtickn=xtn
	
	if keyword_set(redgearr8) then begin
		oplot,redgearr8[rind,*],redgearr8[rind+1,*],color=clr(0)
	endif

	if keyword_set(redgearr9) then begin
		oplot,redgearr9[rind,*]+dt9,redgearr9[rind+1,*],color=clr(1)
	endif
endif

if keyword_set(redgearr6) || keyword_set(redgearr7) then begin
	yr = [rmin67,rmax67] + (rmax67-rmin67)*[-.1,.1]
	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[3], xtickn=xtn
	
	if keyword_set(redgearr6) then begin
		oplot,redgearr6[rind,*],redgearr6[rind+1,*],color=clr(0)
	endif

	if keyword_set(redgearr7) then begin
		oplot,redgearr7[rind,*]+dt7,redgearr7[rind+1,*],color=clr(1)
	endif
endif

if keyword_set(redgearr4) || keyword_set(redgearr5) then begin
	yr = [rmin45,rmax45] + (rmax45-rmin45)*[-.1,.1]
	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[2], xtickn=xtn
	
	if keyword_set(redgearr4) then begin
		oplot,redgearr4[rind,*],redgearr4[rind+1,*],color=clr(0)
	endif

	if keyword_set(redgearr5) then begin
		oplot,redgearr5[rind,*]+dt5,redgearr5[rind+1,*],color=clr(1)
	endif
endif

case imagenum of
  0: begin
    redgearr1a = redgearr1
    redgearr1a[rind,*] = sigma_filter( redgearr1a[rind,*] + dt1, 100 )
    redgearr3a = redgearr3
    redgearr3a[rind,*] = sigma_filter( redgearr3a[rind,*] + dt3, 100 )
  end 
  1: begin
    redgearr1b = redgearr1
    redgearr1b[rind,*] = sigma_filter( redgearr1b[rind,*] + dt1, 100 )
    redgearr3b = redgearr3
    redgearr3b[rind,*] = sigma_filter( redgearr3b[rind,*] + dt3, 100 )
  end 
  2: begin
    redgearr1c = redgearr1
    redgearr1c[rind,*] = sigma_filter( redgearr1c[rind,*] + dt1, 100 )
    redgearr3c = redgearr3
    redgearr3c[rind,*] = sigma_filter( redgearr3c[rind,*] + dt3, 100 )
  end 
  3: begin
    redgearr1d = redgearr1
    redgearr1d[rind,*] = sigma_filter( redgearr1d[rind,*] + dt1, 100 )
    redgearr3d = redgearr3
    redgearr3d[rind,*] = sigma_filter( redgearr3d[rind,*] + dt3, 100 )
  end 
  4: begin
    redgearr1e = redgearr1
    redgearr1e[rind,*] = sigma_filter( redgearr1e[rind,*] + dt1, 100 )
    redgearr3e = redgearr3
    redgearr3e[rind,*] = sigma_filter( redgearr3e[rind,*] + dt3, 100 )
  end 
  5: begin
    redgearr1f = redgearr1
    redgearr1f[rind,*] = sigma_filter( redgearr1f[rind,*] + dt1, 100 )
    redgearr3f = redgearr3
    redgearr3f[rind,*] = sigma_filter( redgearr3f[rind,*] + dt3, 100 )
  end 
  6: begin
    redgearr1g = redgearr1
    redgearr1g[rind,*] = sigma_filter( redgearr1g[rind,*] + dt1, 100 )
    redgearr3g = redgearr3
    redgearr3g[rind,*] = sigma_filter( redgearr3g[rind,*] + dt3, 100 )
  end 
  7: begin
    redgearr1h = redgearr1
    redgearr1h[rind,*] = sigma_filter( redgearr1h[rind,*] + dt1, 100 )
    redgearr3h = redgearr3
    redgearr3h[rind,*] = sigma_filter( redgearr3h[rind,*] + dt3, 100 )
  end 
  8: begin
    redgearr1i = redgearr1
    redgearr1i[rind,*] = sigma_filter( redgearr1i[rind,*] + dt1, 100 )
    redgearr3i = redgearr3
    redgearr3i[rind,*] = sigma_filter( redgearr3i[rind,*] + dt3, 100 )
  end 
endcase

if keyword_set(redgearr2) || keyword_set(redgearr3) then begin
	yr = [rmin23,rmax23] + (rmax23-rmin23)*[-.1,.1]
	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[1], xtickn=xtn
	if imagenum eq 8 then begin
          oplot, redgearr3a[rind,*], redgearr3a[rind+1,*], color=clr1[0]
          oplot, redgearr3b[rind,*], redgearr3b[rind+1,*], color=clr1[1]
          oplot, redgearr3c[rind,*], redgearr3c[rind+1,*], color=clr1[2]
          oplot, redgearr3d[rind,*], redgearr3d[rind+1,*], color=clr1[3]
          oplot, redgearr3e[rind,*], redgearr3e[rind+1,*], color=clr1[4]
          oplot, redgearr3f[rind,*], redgearr3f[rind+1,*], color=clr1[5]
          oplot, redgearr3g[rind,*], redgearr3g[rind+1,*], color=clr1[6]
          oplot, redgearr3h[rind,*], redgearr3h[rind+1,*], color=clr1[7]
          oplot, redgearr3i[rind,*], redgearr3i[rind+1,*], color=clr1[8]
        endif else begin
	  if keyword_set(redgearr2) then begin
		oplot,redgearr2[rind,*],redgearr2[rind+1,*],color=clr(0)
          endif
        endelse

	if keyword_set(redgearr3) then begin
		oplot,redgearr3[rind,*]+dt3,redgearr3[rind+1,*],color=clr(1)
	endif
endif

;  This is a hack and will not work when Encke Inner is not the last one plotted
xtn = '' 
xtit = 'Co-Rotating Longitude (!Uo!N)'

if keyword_set(redgearr0) || keyword_set(redgearr1) then begin
	yr = [rmin01,rmax01] + (rmax01-rmin01)*[-.1,.1]
	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[0], xtickn=xtn
	
	if imagenum eq 8 then begin
          oplot, redgearr1a[rind,*], redgearr1a[rind+1,*], color=clr1[0]
          oplot, redgearr1b[rind,*], redgearr1b[rind+1,*], color=clr1[1]
          oplot, redgearr1c[rind,*], redgearr1c[rind+1,*], color=clr1[2]
          oplot, redgearr1d[rind,*], redgearr1d[rind+1,*], color=clr1[3]
          oplot, redgearr1e[rind,*], redgearr1e[rind+1,*], color=clr1[4]
          oplot, redgearr1f[rind,*], redgearr1f[rind+1,*], color=clr1[5]
          oplot, redgearr1g[rind,*], redgearr1g[rind+1,*], color=clr1[6]
          oplot, redgearr1h[rind,*], redgearr1h[rind+1,*], color=clr1[7]
          oplot, redgearr1i[rind,*], redgearr1i[rind+1,*], color=clr1[8]
        endif else begin
	  if keyword_set(redgearr0) then begin
		oplot,redgearr0[rind,*],redgearr0[rind+1,*],color=clr(0)
          endif
        endelse

	if keyword_set(redgearr1) then begin
		oplot,redgearr1[rind,*]+dt1,redgearr1[rind+1,*],color=clr(1)
	endif
endif

mainmenu:
reply1 = ''
while reply1 eq '' do begin
  ;print, 'dr = '+strtrim(dr,2)+', dl = '+strtrim(dl,2)+', dtwist = '+strtrim(dtwist,2)+', dt1 = 'strtrim(dt1,2)+', dt3 = 'strtrim(dt3,2)+', dt5 = 'strtrim(dt5,2)+', dt7 = 'strtrim(dt7,2)+', dt9 = '+strtrim(dt9,2)
  print, 'dr, dl, dtwist, dt1, dt3, dt5, dt7, dt9'
  print, dr, dl, dtwist, dt1, dt3, dt5, dt7, dt9
  print, 'ADJUST [r]adius, [l]ongitude, t[w]ist, various [t]imes, return to [o]riginal pointing,'
  print, '[a]dd another image, [c]reate PS image, [z] save global .dat file, [h]elp, '
  ;print, '(a/r/l/w/t/o/c/z/q/h)'
  read, reply1
;if imagenum lt 8 then reply1='a' else read, reply1
  case reply1 of
    'r': type = [ 'radial', 'pixels' ]
    'l': type = [ 'longitudinal', 'pixels' ]
    'w': type = [ 'twist', 'milliradians' ]
    't': type = [ 'time', 'degrees of longitude' ]
    'a': begin

         if keyword_set(EnckeInner) then begin
		 EInew=0
	 endif
	
	 if keyword_set(EnckeOuter) then begin
		 EOnew=0
	 endif

	 if keyword_set(KeelerInner) then begin
		 KInew=0
	 endif

	 if keyword_set(KeelerOuter) then begin
		 KOnew=0
	 endif

	 if keyword_set(Roche) then begin
		 Rnew=0
	 endif

         goto, add
     end
    'h': begin
         print, 'Image name, image number:'
         print, imageNEW, imagenum
         goto, mainmenu
     end
    'c': goto, postscript
    'q': goto, quit
    'z': goto, globalmap
    'o': begin
        dr = 0
        dt1 = .007245778*(_et[0]-_et[imagenum]) ;motion due to Pan
        ;dt3 = 0
        dt3 = .007245778*(_et[0]-_et[imagenum]) ;motion due to Pan
        dt5 = 0
        dt7 = 0
        dt9 = 0
        ;;;Note that o changes the graphing variables, but not the graph. Choose to adjust another parameter by 0 to see these changes.
        goto, mainmenu
    end
    else: reply1 = ''
  endcase
endwhile

reply2 = ''
print, 'Enter '+type[0]+' offset in '+type[1]+' (or "q" to quit).'
read, reply2
if reply2 eq 'q' then goto, quit

reply2 = float(reply2)

_dr = 0.
_dl = 0.
_dtwist = 0.
case reply1 of
  'r': _dr = reply2
  'l': _dl = reply2
  'w': _dtwist = reply2 / 1d3
  't': begin
      reply4 = ''
      while reply4 eq '' do begin
          ;print, 'The following are values for dt1, dt3, dt5, dt7, dt9:'
          ;print, dt1, dt3, dt5, dt7, dt9
          print, 'ADJUST time for [1]Encke Inner, [3]Encke Outer, [5]Keeler Inner, '
          print, '[7]Keeler Outer, [9]Outer Edge of A Ring, or [q]uit.'
          ;print, '(1/3/5/7/9/q)'
          read, reply4
            case reply4 of
              '1': dt1 = dt1 + reply2
              '3': dt3 = dt3 + reply2
              '5': dt5 = dt5 + reply2
              '7': dt7 = dt7 + reply2
              '9': dt9 = dt9 + reply2
              'q': goto, quit
              else: reply4 = ''
          endcase
      endwhile
  end
endcase
dr = dr + _dr
dl = dl + _dl
dtwist = dtwist + _dtwist

;--------------------------------------------------------------------;

adjust:
; Use draw_arrows.pro to find the radial and longitudinal directions
arr_len = 1
if keyword_exists(noplot) then oldnp = noplot else oldnp = 0
noplot = 1
@draw_arrows
noplot = oldnp

; Use input dr and/or dl to move image pointing (i.e. change cmat)
x_move = (arr_sat_coords[0,1]-arr_sat_coords[1,1])*_dr + $
         (arr_orb_coords[1,1]-arr_orb_coords[0,1])*_dl
y_move = (arr_sat_coords[0,0]-arr_sat_coords[1,0])*_dr + $
         (arr_orb_coords[1,0]-arr_orb_coords[0,0])*_dl
@move_bypixel

; Uses input dtwist to change cmat
@cmat_dtwist

if keyword_set(redgearr1) then begin
  redge_recalc = linsam_to_redge_encke(cmat,redgearr1[0:1,*],redgearr1[2:3,*],redgearr1[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  redgearr1[10:11,*] = redge_recalc
  redgearr1[12:13,*] = redge2_sigma
endif

if keyword_set(redgearr3) then begin
  redge_recalc = linsam_to_redge_encke(cmat,redgearr3[0:1,*],redgearr3[2:3,*],redgearr3[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  redgearr3[10:11,*] = redge_recalc
  redgearr3[12:13,*] = redge2_sigma 
endif

if keyword_set(redgearr5) then begin
  redge_recalc = linsam_to_redge_encke(cmat,redgearr5[0:1,*],redgearr5[2:3,*],redgearr5[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  redgearr5[10:11,*] = redge_recalc
  redgearr5[12:13,*] = redge2_sigma 
endif

if keyword_set(redgearr7) then begin
  redge_recalc = linsam_to_redge_encke(cmat,redgearr7[0:1,*],redgearr7[2:3,*],redgearr7[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  redgearr7[10:11,*] = redge_recalc
  redgearr7[12:13,*] = redge2_sigma 
endif

if keyword_set(redgearr9) then begin
  redge_recalc = linsam_to_redge_encke(cmat,redgearr9[0:1,*],redgearr9[2:3,*],redgearr9[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  redgearr9[10:11,*] = redge_recalc
  redgearr9[12:13,*] = redge2_sigma 
endif


goto, nextplot

moveplotandexit:


postscript:

if keyword_set(redgearr1) then redgearr1(rind,*) = redgearr1(rind,*)+dt1
if keyword_set(redgearr3) then redgearr3(rind,*) = redgearr3(rind,*)+dt3
if keyword_set(redgearr5) then redgearr5(rind,*) = redgearr5(rind,*)+dt5
if keyword_set(redgearr7) then redgearr7(rind,*) = redgearr7(rind,*)+dt7
if keyword_set(redgearr9) then redgearr9(rind,*) = redgearr9(rind,*)+dt9

if keyword_set(redgearr1) then redgearr0 = [[redgearr0],[redgearr1]]
if keyword_set(redgearr3) then redgearr2 = [[redgearr2],[redgearr3]]
if keyword_set(redgearr5) then redgearr4 = [[redgearr4],[redgearr5]]
if keyword_set(redgearr7) then redgearr6 = [[redgearr6],[redgearr7]]
if keyword_set(redgearr9) then redgearr8 = [[redgearr8],[redgearr9]]

if keyword_set(redgearr0) then begin
	redgearr0 = redgearr0[*,sort(redgearr0(rind,*))]
	meadrad0 = median(redgearr0(rind+1,*))
	SD0 = stddev(redgearr0(rind+1,*))
endif

if keyword_set(redgearr2) then begin
	redgearr2 = redgearr2[*,sort(redgearr2(rind,*))]
	meadrad2 = median(redgearr2(rind+1,*))
	SD2 = stddev(redgearr2(rind+1,*))
endif

if keyword_set(redgearr4) then begin
	redgearr4 = redgearr4[*,sort(redgearr4(rind,*))]
	meadrad4 = median(redgearr4(rind+1,*))
	SD4 = stddev(redgearr4(rind+1,*))
endif


if keyword_set(redgearr6) then begin
	redgearr6 = redgearr6[*,sort(redgearr6(rind,*))]
	meadrad6 = median(redgearr6(rind+1,*))
	SD6 = stddev(redgearr6(rind+1,*))
endif

if keyword_set(redgearr8) then begin
	redgearr8 = redgearr8[*,sort(redgearr8(rind,*))]
	meadrad8 = median(redgearr8(rind+1,*))
	SD8 = stddev(redgearr8(rind+1,*))
endif


lzr
@plot_prepare

; X Limits for Inner and Outer Edge (constant for both)
xtn = replicate(' ',20) 
xtit = ' '
;xtit = 'Co-Rotating Longitude (!Uo!N)'
ytit = ['Encke Gap Inner Edge' ,'Encke Gap Outer Edge','Keeler Gap Inner Edge','Keeler Gap Outer Edge','Roche Gap Edge']

lmin = 1000
lmax = -1000
SDmax = 0

if keyword_set(redgearr0) then begin
	lmin = min( [lmin, min(redgearr0[rind,*]) ] )
	lmax = max( [lmax, max(redgearr0[rind,*]) ] )
	SDmax = max( [SDmax,SD0] )
endif

if keyword_set(redgearr2) then begin
	lmin = min( [ lmin, min(redgearr2[rind,*]) ])
	lmax = max( [ lmax, max(redgearr2[rind,*]) ])
	SDmax = max( [SDmax,SD2] )
endif

if keyword_set(redgearr4) then begin
	lmin = min( [ lmin, min(redgearr4[rind,*]) ] )
	lmax = max( [ lmax, max(redgearr4[rind,*]) ] )
	SDmax = max( [SDmax,SD4] )
endif

if keyword_set(redgearr6) then begin
	lmin = min( [ lmin, min(redgearr6[rind,*]) ])
	lmax = max( [ lmax, max(redgearr6[rind,*]) ])
	SDmax = max( [SDmax,SD6] )
endif

if keyword_set(redgearr8) then begin
	lmin = min( [ lmin, min(redgearr8[rind,*]) ] )
	lmax = max( [ lmax, max(redgearr8[rind,*]) ] )
	SDmax = max( [SDmax,SD8] )
endif

sat=635
@get_sat_coords
moonlong=0;  171.3 ;sat_polar(1)

xr = [lmin,lmax] + (lmax-lmin)*[-.1,.1] - moonlong

if keyword_set(redgearr8) then begin
	redgearr8(rind,*) = redgearr8(rind,*)-moonlong
	yr = [meadrad8-3*SDmax, meadrad8+3*SDmax] 

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[4], xtickn=xtn
	oplot,redgearr8[rind,*],redgearr8[rind+1,*]
  	openw,1,'AGap.dat'
    	for j=0l,(size(redgearr8))(2)-1 do begin
       		printf,1,redgearr8(rind,j),redgearr8(rind+1,j),redgearr8(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif

if keyword_set(redgearr6) then begin
	redgearr6(rind,*) = redgearr6(rind,*)-moonlong
	yr = [meadrad6-3*SDmax, meadrad6+3*SDmax]

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[3], xtickn=xtn
	oplot,redgearr6[rind,*],redgearr6[rind+1,*]
  	openw,1,'KeelerGapOuter.dat'
    	for j=0l,(size(redgearr6))(2)-1 do begin
       		printf,1,redgearr6(rind,j),redgearr6(rind+1,j),redgearr6(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif

if keyword_set(redgearr4) then begin
	redgearr4(rind,*) = redgearr4(rind,*)-moonlong
	yr = [meadrad4-3*SDmax, meadrad4+3*SDmax]

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[2], xtickn=xtn
	oplot,redgearr4[rind,*],redgearr4[rind+1,*]
  	openw,1,'KeelerGapInner.dat'
    	for j=0l,(size(redgearr4))(2)-1 do begin
       		printf,1,redgearr4(rind,j),redgearr4(rind+1,j),redgearr4(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif

if keyword_set(redgearr2) then begin
	redgearr2(rind,*) = redgearr2(rind,*)-moonlong
	yr = [meadrad2-3*SDmax, meadrad2+3*SDmax]

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[1], xtickn=xtn
	oplot,redgearr2[rind,*],redgearr2[rind+1,*]
  	openw,1,'EnckeGapOuter.dat'
    	for j=0l,(size(redgearr2))(2)-1 do begin
       		printf,1,redgearr2(rind,j),redgearr2(rind+1,j),redgearr2(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif

;  This is a hack and will not work when Encke Inner is not the last one plotted
xtn = '' 
xtit = 'Co-Rotating Longitude (!Uo!N)'

if keyword_set(redgearr0) then begin
	redgearr0(rind,*) = redgearr0(rind,*)-moonlong
	yr = [meadrad0-3*SDmax, meadrad0+3*SDmax] 

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[0], xtickn=xtn
	oplot,redgearr0[rind,*],redgearr0[rind+1,*]
  	openw,1,'EnckeGapInner.dat'
    	for j=0l,(size(redgearr0))(2)-1 do begin
       		printf,1,redgearr0(rind,j),redgearr0(rind+1,j),redgearr0(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif

clzr

goto,quit

if 0 then begin

globalmap:
  
@bootstrap_initialize

firstpic=''
lastpic =''
read,firstpic,prompt='First Included Picture Number : '
read,lastpic,prompt ='Last Included Picture Number : '
firstpic = fix(firstpic)
lastpic  = fix(lastpic)


KInew=1
KOnew=1
EInew=1
EOnew=1
Rnew =1



for i=firstpic,lastpic do begin
    if (i eq 26 ) then i = i+2
    imageNEW=filenames(i)
    periodNEW= rstrpos(imageNEW,'.')
    stemNEW  = strmid(imageNEW,0,periodNEW)
    files = file_search(stemNEW+'.edge_linsam*')
    if( (size(files))(0) eq 0) then stop
    nedges = (size(files))(1);
    titles = strmid(files,29,2)
    KeelerInner = max(titles eq 'KI')
    KeelerOuter= max(titles eq 'KO')
    EnckeInner  = max(titles eq 'EI')
    EnckeOuter = max(titles eq 'EO')
    Roche      = max(titles eq 'R')

;------------------ Import New Edges ----------------------------;

	if keyword_set(EnckeInner) then begin
		restore, stemNEW+'.edge_linsamEI'
		redge1 = linsam_to_redge_encke( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
		redgearr1 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	endif

	if keyword_set(EnckeOuter) then begin
		restore, stemNEW+'.edge_linsamEO'
		redge1 = linsam_to_redge_encke( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
		redgearr3 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	endif

	if keyword_set(KeelerInner) then begin
		restore, stemNEW+'.edge_linsamKI'
		redge1 = linsam_to_redge_encke( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
		redgearr5 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	endif

	if keyword_set(KeelerOuter) then begin
		restore, stemNEW+'.edge_linsamKO'
		redge1 = linsam_to_redge_encke( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
		redgearr7 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	endif

	if keyword_set(Roche) then begin
		restore, stemNEW+'.edge_linsamR'
		redge1 = linsam_to_redge_encke( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
		redgearr9 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	endif

	cmat = REDGE_CMAT

; -------------------- Filter New Edges -----------------------------;
filtersize= 2.5

	if keyword_set(redgearr1) then begin
		redgearr1 = medianfilter(redgearr1,filtersize,rind)
	endif

	if keyword_set(redgearr3) then begin
		redgearr3 = medianfilter(redgearr3,filtersize,rind)
	endif

	if keyword_set(redgearr5) then begin
		redgearr5 = medianfilter(redgearr5,filtersize,rind)
	endif

	if keyword_set(redgearr7) then begin
		redgearr7 = medianfilter(redgearr7,filtersize,rind)
	endif

	if keyword_set(Roche) then begin
		redgearr9 = medianfilter(redgearr9,filtersize,rind)
	endif

;-------------------------------------------------------------------;


    	restore, imageNEW+'_bsa'
        _dr = dr
    	;_dt = dt
        _dt1 = dt1
        _dt3 = dt3
        _dt5 = dt5
        _dt7 = dt7
        _dt9 = dt9
    	_dl = dl
        _dtwist = dtwist
    	arr_len = 1
    	noplot = 1
    	@draw_arrows
    	x_move = (arr_sat_coords[0,1]-arr_sat_coords[1,1])*_dr + (arr_orb_coords[1,1]-arr_orb_coords[0,1])*_dl
    	y_move = (arr_sat_coords[0,0]-arr_sat_coords[1,0])*_dr + (arr_orb_coords[1,0]-arr_orb_coords[0,0])*_dl
    	@move_bypixel

	if keyword_set(redgearr1) then begin
  		redge_recalc = linsam_to_redge_encke(cmat,redgearr1[0:1,*],redgearr1[2:3,*],redgearr1[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  		redgearr1[10:11,*] = redge_recalc
		redgearr1[12:13,*] = redge2_sigma
	endif
	if keyword_set(redgearr3) then begin
  		redge_recalc = linsam_to_redge_encke(cmat,redgearr3[0:1,*],redgearr3[2:3,*],redgearr3[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  		redgearr3[10:11,*] = redge_recalc
  		redgearr3[12:13,*] = redge2_sigma 
	endif
	if keyword_set(redgearr5) then begin
  		redge_recalc = linsam_to_redge_encke(cmat,redgearr5[0:1,*],redgearr5[2:3,*],redgearr5[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  		redgearr5[10:11,*] = redge_recalc
  		redgearr5[12:13,*] = redge2_sigma 
	endif
	if keyword_set(redgearr7) then begin
  		redge_recalc = linsam_to_redge_encke(cmat,redgearr7[0:1,*],redgearr7[2:3,*],redgearr7[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  		redgearr7[10:11,*] = redge_recalc
  		redgearr7[12:13,*] = redge2_sigma 
	endif
	if keyword_set(redgearr9) then begin
		redge_recalc = linsam_to_redge_encke(cmat,redgearr9[0:1,*],redgearr9[2:3,*],redgearr9[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  		redgearr9[10:11,*] = redge_recalc
  		redgearr9[12:13,*] = redge2_sigma
	endif



if not keyword_set(EInew) then begin
  if keyword_set(EnckeInner) then   redgearr1(rind,*) = redgearr1(rind,*)+dt1
  if keyword_set(EnckeInner) then   redgearr0 = [[redgearr0],[redgearr1]]
  if keyword_set(EnckeInner) then   redgearr0 = redgearr0[*,sort(redgearr0(rind,*))]
endif

if not keyword_set(EOnew) then begin
  if keyword_set(EnckeOuter) then  redgearr3(rind,*) = redgearr3(rind,*)+dt3
  if keyword_set(EnckeOuter) then  redgearr2 = [[redgearr2],[redgearr3]]
  if keyword_set(EnckeOuter) then  redgearr2 = redgearr2[*,sort(redgearr2(rind,*))]
endif

if not keyword_set(KInew) then begin
  if keyword_set(KeelerInner) then  redgearr5(rind,*) = redgearr5(rind,*)+dt5
  if keyword_set(KeelerInner) then  redgearr4 = [[redgearr4],[redgearr5]]
  if keyword_set(KeelerInner) then  redgearr4 = redgearr4[*,sort(redgearr4(rind,*))]
endif

if not keyword_set(KOnew) then begin
  if keyword_set(KeelerOuter) then redgearr7(rind,*) = redgearr7(rind,*)+dt7
  if keyword_set(KeelerOuter) then redgearr6 = [[redgearr6],[redgearr7]]
  if keyword_set(KeelerOuter) then redgearr6 = redgearr6[*,sort(redgearr6(rind,*))]
endif

if not keyword_set(Rnew) then begin
  if keyword_set(Roche)   then     redgearr9(rind,*) = redgearr9(rind,*)+dt9
  if keyword_set(Roche)    then    redgearr8 = [[redgearr8],[redgearr9]]
  if keyword_set(Roche)   then     redgearr8 = redgearr8[*,sort(redgearr8(rind,*))]
endif


if keyword_set(EInew) then begin
  if keyword_set(EnckeInner) then   redgearr0 = redgearr1
  if keyword_set(EnckeInner) then   redgearr0(rind,*) = redgearr0(rind,*)+dt1
  if keyword_set(EnckeInner) then EInew=0
endif 

if keyword_set(EOnew) then begin
  if keyword_set(EnckeOuter) then  redgearr2 = redgearr3
  if keyword_set(EnckeOuter) then  redgearr2(rind,*) = redgearr2(rind,*)+dt3
  if keyword_set(EnckeOuter) then EOnew=0
endif

if keyword_set(KInew) then begin
  if keyword_set(KeelerInner) then  redgearr4 = redgearr5
  if keyword_set(KeelerInner) then  redgearr4(rind,*) = redgearr4(rind,*)+dt5
  if keyword_set(KeelerInner) then KInew=0
endif

if keyword_set(KOnew) then begin
  if keyword_set(KeelerOuter) then redgearr6 = redgearr7
  if keyword_set(KeelerOuter) then redgearr6(rind,*) = redgearr6(rind,*)+dt7
  if keyword_set(KeelerOuter) then KOnew=0
endif

if keyword_set(Rnew) then begin
  if keyword_set(Roche)   then     redgearr8 = redgearr9
  if keyword_set(Roche) then       redgearr8(rind,*) = redgearr8(rind,*)+dt9
  if keyword_set(Roche) then Rnew=0
endif


print,i
endfor

smoothinglength = 2000

if keyword_set(redgearr6) then begin
	fix =  136522.0 - smooth( redgearr6(rind+1,*),smoothinglength,edge_truncate=1 );
	minsize = min( [(size(redgearr4))(2), (size(redgearr6))(2) ]) -1;  min( [(size(redgearr0))(2), (size(redgearr2))(2), (size(redgearr4))(2) , (size(redgearr6))(2) , (size(redgearr8))(2) ] )
	fix = fix[0:minsize]
endif

if keyword_set(redgearr0) then begin
	redgearr0(rind+1,*) = redgearr0(rind+1,*)
	redgearr0 = redgearr0[*,sort(redgearr0(rind,*))]
	meadrad0 = median(redgearr0(rind+1,*))
	SD0 = stddev(redgearr0(rind+1,*))
endif

if keyword_set(redgearr2) then begin
	redgearr2(rind+1,*) = redgearr2(rind+1,*)
	redgearr2 = redgearr2[*,sort(redgearr2(rind,*))]
	meadrad2 = median(redgearr2(rind+1,*))
	SD2 = stddev(redgearr2(rind+1,*))
endif

if keyword_set(redgearr4) then begin
	redgearr4 = redgearr4(*,0:minsize)
	redgearr4(rind+1,*) = redgearr4(rind+1,*)+fix
	redgearr4 = redgearr4[*,sort(redgearr4(rind,*))]
	meadrad4 = median(redgearr4(rind+1,*))
	SD4 = stddev(redgearr4(rind+1,*))
endif


if keyword_set(redgearr6) then begin
	redgearr6 = redgearr6(*,0:minsize)
	redgearr6(rind+1,*) = redgearr6(rind+1,*)+fix
	redgearr6 = redgearr6[*,sort(redgearr6(rind,*))]
	meadrad6 = median(redgearr6(rind+1,*))
	SD6 = stddev(redgearr6(rind+1,*))
endif

if keyword_set(redgearr8) then begin
	redgearr8 = redgearr8(*,0:minsize)
	redgearr8(rind+1,*) = redgearr8(rind+1,*)+fix
	redgearr8 = redgearr8[*,sort(redgearr8(rind,*))]
	meadrad8 = median(redgearr8(rind+1,*))
	SD8 = stddev(redgearr8(rind+1,*))
endif

lzr
@plot_prepare


xtn = replicate(' ',20) 
xtit = ' '
;xtit = 'Co-Rotating Longitude (!Uo!N)'
ytit = ['Encke Gap Inner Edge' ,'Encke Gap Outer Edge','Keeler Gap Inner Edge','Keeler Gap Outer Edge','Roche Gap Edge']

lmin = 1000
lmax = -1000
SDmax = 0

if keyword_set(redgearr0) then begin
	lmin = min( [lmin, min(redgearr0[rind,*]) ] )
	lmax = max( [lmax, max(redgearr0[rind,*]) ] )
	SDmax = max( [SDmax,SD0] )
endif

if keyword_set(redgearr2) then begin
	lmin = min( [ lmin, min(redgearr2[rind,*]) ])
	lmax = max( [ lmax, max(redgearr2[rind,*]) ])
	SDmax = max( [SDmax,SD2] )
endif

if keyword_set(redgearr4) then begin
	lmin = min( [ lmin, min(redgearr4[rind,*]) ] )
	lmax = max( [ lmax, max(redgearr4[rind,*]) ] )
	SDmax = max( [SDmax,SD4] )
endif

if keyword_set(redgearr6) then begin
	lmin = min( [ lmin, min(redgearr6[rind,*]) ])
	lmax = max( [ lmax, max(redgearr6[rind,*]) ])
	SDmax = max( [SDmax,SD6] )
endif

if keyword_set(redgearr8) then begin
	lmin = min( [ lmin, min(redgearr8[rind,*]) ] )
	lmax = max( [ lmax, max(redgearr8[rind,*]) ] )
	SDmax = max( [SDmax,SD8] )
endif

moonlong = 0

xr = [lmin,lmax] + (lmax-lmin)*[-.1,.1]


if keyword_set(redgearr8) then begin
	redgearr8(rind,*) = redgearr8(rind,*)-moonlong
	yr = [meadrad8-3*SDmax, meadrad8+3*SDmax] 

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[4], xtickn=xtn
	oplot,redgearr8[rind,*],redgearr8[rind+1,*]
  	openw,1,'AGap.dat'
    	for j=0l,(size(redgearr8))(2)-1 do begin
       		printf,1,redgearr8(rind,j),redgearr8(rind+1,j),redgearr8(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif


if keyword_set(redgearr6) then begin
	redgearr4(rind,*) = redgearr4(rind,*)-moonlong
	yr = [meadrad6-3*SDmax, meadrad6+3*SDmax]

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[3], xtickn=xtn
	oplot,redgearr6[rind,*],redgearr6[rind+1,*]
  	openw,1,'KeelerGapOuter.dat'
    	for j=0l,(size(redgearr6))(2)-1 do begin
       		printf,1,redgearr6(rind,j),redgearr6(rind+1,j),redgearr6(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif

if keyword_set(redgearr4) then begin
	redgearr6(rind,*) = redgearr6(rind,*)-moonlong
	yr = [meadrad4-3*SDmax, meadrad4+3*SDmax]

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[2], xtickn=xtn
	oplot,redgearr4[rind,*],redgearr4[rind+1,*]
  	openw,1,'KeelerGapInner.dat'
    	for j=0l,(size(redgearr4))(2)-1 do begin
       		printf,1,redgearr4(rind,j),redgearr4(rind+1,j),redgearr4(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif


if keyword_set(redgearr2) then begin
	redgearr2(rind,*) = redgearr2(rind,*)-moonlong
	yr = [meadrad2-3*SDmax, meadrad2+3*SDmax]

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[1], xtickn=xtn
	oplot,redgearr2[rind,*],redgearr2[rind+1,*]
  	openw,1,'EnckeGapOuter.dat'
    	for j=0l,(size(redgearr2))(2)-1 do begin
       		printf,1,redgearr2(rind,j),redgearr2(rind+1,j),redgearr2(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif

;  This is a hack and will not work when the Roche gap is not the last one plotted
xtn = '' 
xtit = 'Co-Rotating Longitude (!Uo!N)'

if keyword_set(redgearr0) then begin
	redgearr0(rind,*) = redgearr0(rind,*)-moonlong
	yr = [meadrad0-3*SDmax, meadrad0+3*SDmax] 

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[0], xtickn=xtn
	oplot,redgearr0[rind,*],redgearr0[rind+1,*]
  	openw,1,'EnckeGapInner.dat'
    	for j=0l,(size(redgearr0))(2)-1 do begin
       		printf,1,redgearr0(rind,j),redgearr0(rind+1,j),redgearr0(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif

;writedata = ''
;read,writedata,prompt='Write .dat files? : [y/n]'

;if (writedata eq 'y' || writedata eq 'Y') then begin
;	dataset =''
;	read,dataset,prompt='Base File Name : '
;
;  	if keyword_set(redgearr0) then begin
;		openw,1,dataset+'EI.dat'
;	    	for j=0l,(size(redgearr0))(2)-1 do begin
 ;     			printf,1,redgearr0(rind,j),redgearr0(rind+1,j),redgearr0(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
  ;  		endfor
  ;		close,1
;	endif
;
 ; 	if keyword_set(redgearr2) then begin
;		openw,1,dataset+'EO.dat'
;	    	for j=0l,(size(redgearr2))(2)-1 do begin
 ;     			printf,1,redgearr2(rind,j),redgearr2(rind+1,j),redgearr2(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
  ;  		endfor
  ;		close,1
;	endif
;
;
;
 ; 	if keyword_set(redgearr4) then begin
;		openw,1,dataset+'KI.dat'
;	    	for j=0l,(size(redgearr4))(2)-1 do begin
 ;     			printf,1,redgearr4(rind,j),redgearr4(rind+1,j),redgearr4(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
  ;  		endfor
  ;		close,1
;	endif
;
 ; 	if keyword_set(redgearr6) then begin
;		openw,1,dataset+'KO.dat'
;	    	for j=0l,(size(redgearr6))(2)-1 do begin
 ;     			printf,1,redgearr6(rind,j),redgearr6(rind+1,j),redgearr6(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
  ;  		endfor
  ;		close,1
;	endif
;
;
 ; 	if keyword_set(redgearr8) then begin
;		openw,1,dataset+'R.dat'
;	    	for j=0l,(size(redgearr8))(2)-1 do begin
 ;     			printf,1,redgearr8(rind,j),redgearr8(rind+1,j),redgearr8(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
  ;  		endfor
  ;		close,1
;	endif
;
;endif

@bootstrap_dat

clzr


endif

quit:
wdelete
!x.margin = [10,3]
!y.margin = [4,2]
!y.omargin = [0,0]

end
