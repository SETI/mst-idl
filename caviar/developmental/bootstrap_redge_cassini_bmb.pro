;; -------------------- bootstrap_redge_cassini_bmb.pro ------------------ ;;
;; LAST MODIFIED:  P.TORREY 5/15/08. B. BYINGTON 5/11 (comments)
;; DEPENDENCIES:   .edge files for images in sequence
;; 
;; SUMMARY:	   This the the MAIN code.  It takes the individual edge profiles
;;		   which must be stored in the format:
;;			image_name.linsamH  (for Herschel outer)
;;			image_name.linsamJ  (for Jeffreys outer)
;;			image_name.linsamS  (for Russell outer; the
;;			letter R was already taken for Roche division)
;;			image_name.linsamU  (for Kuiper outer; the
;;			letter K was already taken for Keeler edges)
;;		   This code will load these edge profiles, and allow the user to move them according
;;		   in the longitudinal, radial, and time directions.
;;		   Hypothetically, additional edge profiles can be
;;		   added through the interactive menu,
;;                 allowing for global maps to be created. However,
;;                 its main purpose ended being looking at and
;;                 adjusting data from edge profiles for individual
;;                 images only, so users might run into problems using
;;                 the bootstrapping features of the code.
;;
;;This code was adapted from bootstrap_redge_encke_bmb3.pro, which was
;;adapted from P. Torrey's original bootstrap_redge_encke.pro.

firstimage = 1000
lastimage  = 0
rind = 10 ;r index
imagenum = 0

redgearr0=0 ;Herschel - it turns out that this edge is less reliably
                                ;circular than the others, so its not
                                ;actually a reliable fiducial but
                                ;remains in the code
redgearr1=0 
redgearr1o=0 ;offset
redgearr2=0 ;Russell
redgearr3=0 
redgearr3o=0
redgearr4=0 ;Jeffreys
redgearr5=0
redgearr5o=0
redgearr6=0 ;Kuiper
redgearr7=0
redgearr7o=0

Hcount=0
Scount=0
Jcount=0
Ucount=0

Hnew=1
Snew=1
Jnew=1
Unew=1

dr=0
dl=0
dtwist=0
dt1=0
dt3=0
dt5=0
dt7=0
det=0 ;B. Byington tried to be able to change det dynamically
      ;in this program but there were some hiccups, so this was
      ;later abandoned
_dr=0
_dl=0
_dtwist=0
_dt1=0
_dt3=0
_dt5=0
_dt7=0
_det=0

@bootstrap_initialize
help,polera,poledec

device, retain=2
newnacmat=2
j=17  ;B. Byington doesn't recall what this is supposed to do

;-------------------------------------------------------------------------------;


add:

if (Hcount ge 2) then begin
  if keyword_set(Herschel) then   redgearr1(rind,*) = redgearr1(rind,*)+dt1
  if keyword_set(Herschel) then   redgearr0 = [[redgearr0],[redgearr1]]
  if keyword_set(Herschel) then   redgearr0 = redgearr0[*,sort(redgearr0(rind,*))]
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, det,  filename=imageNEW+'_bsa'
endif

if (Scount ge 2) then begin
  if keyword_set(Russell) then  redgearr3(rind,*) = redgearr3(rind,*)+dt3
  if keyword_set(Russell) then  redgearr2 = [[redgearr2],[redgearr3]]
  if keyword_set(Russell) then  redgearr2 = redgearr2[*,sort(redgearr2(rind,*))]
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, det,  filename=imageNEW+'_bsa'
endif

if (Jcount ge 2) then begin
  if keyword_set(Jeffreys) then  redgearr5(rind,*) = redgearr5(rind,*)+dt5
  if keyword_set(Jeffreys) then  redgearr4 = [[redgearr4],[redgearr5]]
  if keyword_set(Jeffreys) then  redgearr4 = redgearr4[*,sort(redgearr4(rind,*))]
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, det,  filename=imageNEW+'_bsa'
endif

if (Ucount ge 2) then begin
  if keyword_set(Kuiper) then redgearr7(rind,*) = redgearr7(rind,*)+dt7
  if keyword_set(Kuiper) then redgearr6 = [[redgearr6],[redgearr7]]
  if keyword_set(Kuiper) then redgearr6 = redgearr6[*,sort(redgearr6(rind,*))]
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, det, filename=imageNEW+'_bsa'
endif

if (Hcount eq 1) then begin
  if keyword_set(Herschel) then   redgearr0 = redgearr1
  if keyword_set(Herschel) then   redgearr0(rind,*) = redgearr0(rind,*)+dt1
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, det,  filename=imageNEW+'_bsa'
endif 

if (Scount eq 1) then begin
  if keyword_set(Russell) then  redgearr2 = redgearr3
  if keyword_set(Russell) then  redgearr2(rind,*) = redgearr2(rind,*)+dt3
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, det,  filename=imageNEW+'_bsa'
endif

if (Jcount eq 1) then begin
  if keyword_set(Jeffreys) then  redgearr4 = redgearr5
  if keyword_set(Jeffreys) then  redgearr4(rind,*) = redgearr4(rind,*)+dt5
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, det,  filename=imageNEW+'_bsa'
endif

if (Ucount eq 1) then begin
  if keyword_set(Kuiper) then redgearr6 = redgearr7
  if keyword_set(Kuiper) then redgearr6(rind,*) = redgearr6(rind,*)+dt7
  save, dr, dl, dtwist, dt1, dt3, dt5, dt7, det,  filename=imageNEW+'_bsa'
endif

if keyword_set(Hnew) and keyword_set(Snew) and keyword_set(Jnew) and keyword_set(Unew) or keyword_set(chooseimage) then begin
  imageNEW=''
  read,imageNEW,prompt='NEW IMAGE NUMBER : '
  imageNEW = fix(imageNEW)
  imagenum = fix(imageNEW)
  if (imagenum ge lastimage)  then lastimage = imagenum
  if (imagenum le firstimage) then firstimage= imagenum
endif else begin
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
Herschel = max(titles eq 'H')
Russell = max(titles eq 'S')
Jeffreys  = max(titles eq 'J')
Kuiper = max(titles eq 'U')

_dr = 0
_dt1 = 0
_dt3 = 0
_dt5 = 0
_dt7 = 0
_dl = 0
_dtwist = 0
_det=0
dr=0
dl=0
dtwist=0
dt1=0
dt3=0
dt5=0
dt7=0
det=0

;------------------ Import New Edges ----------------------------;

retry:

if keyword_set(Herschel) then begin
	restore, stemNEW+'.edge_linsamH'
        __et=_et[imagenum]+det
	redge1 = linsam_to_redge_encke_bmb( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma,  $
                              filestem=stemNEW)
	redgearr1 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
        j=17
        Hcount = Hcount+1
endif else begin
	redgearr1 = 0
endelse

if keyword_set(Russell) then begin
	restore, stemNEW+'.edge_linsamS'
        __et=_et[imagenum]+det
	redge1 = linsam_to_redge_encke_bmb( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
	redgearr3 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	Scount = Scount+1
endif else begin
	redgearr3 = 0
endelse

if keyword_set(Jeffreys) then begin
	restore, stemNEW+'.edge_linsamJ'
        __et=_et[imagenum]+det
	redge1 = linsam_to_redge_encke_bmb( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
	redgearr5 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	Jcount = Jcount+1
endif else begin
	redgearr5=0
endelse

if keyword_set(Kuiper) then begin
	restore, stemNEW+'.edge_linsamU'
        __et=_et[imagenum]+det
	redge1 = linsam_to_redge_encke_bmb( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
	redgearr7 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	Ucount = Ucount+1
endif else begin
	redgearr7 = 0
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


;--------------------- Adjust New Edges -----------------------------;

if keyword_set(findfile(imageNEW+'_bsa')) then begin
  restore, imageNEW+'_bsa'
  print,'_bsa file found and loading...'
  _dr = dr
  _dt1 = dt1
  _dt3 = dt3
  _dt5 = dt5
  _dt7 = dt7
  _dl = dl
  _dtwist = dtwist
  _det = det
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
_det = 0
_det = det ; not sure if this should go here - bmb 3-19
;   This handles the graph parameters 
!p.charsize=1.5
!p.multi = [0,1,nedges] 
!x.margin = [13,3]
!y.margin = [0,0]
!y.omargin = [4,2]


clr = [ ctred(), ctwhite(), ctgreen() ]

;---------------- GRAPHING LOOP -------------------;

; X Limits for Inner and Outer Edge (constant for both)
xtn = replicate(' ',20) 
xtit = ' '
;xtit = 'Co-Rotating Longitude (!Uo!N)'
ytit = ['Herschel Gap Outer Edge' ,'Russell Gap Outer Edge','Jeffreys Gap Outer Edge','Kuiper Gap Outer Edge']

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


xr = [lmin,lmax] + (lmax-lmin)*[-.1,.1]

if keyword_set(redgearr6) || keyword_set(redgearr7) then begin
	yr = [rmin67,rmax67] + (rmax67-rmin67)*[-.1,.1]
        ;yr = [119404.1, 119408.1]
	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[3], xtickn=xtn
	
	if keyword_set(redgearr6) then begin
		oplot,redgearr6[rind,*],redgearr6[rind+1,*],color=clr(0)
                mean6=119406.1 ;Kuiper Gap outer edge radius
                mean6=redgearr6[rind+1,*]*0+mean6
                oplot, xr, mean6, color=clr(2) ;This plots the expected radius
                                               ;given by Hedman et al. for easier fitting
                                ;oplot, xr, mean6 + .7, color=clr(0)
                ;;This was from when I was trying to fit all four
                ;;edges at once
	endif

	if keyword_set(redgearr7) then begin
		oplot,redgearr7[rind,*]+dt7,redgearr7[rind+1,*],color=clr(1)
                mean6=119406.1  ;Kuiper Gap outer edge radius
                mean6=redgearr7[rind+1,*]*0+mean6
                oplot, xr, mean6, color=clr(2)
                ;oplot, xr, mean6 + .7, color=clr(0)
	endif
endif

if keyword_set(redgearr4) || keyword_set(redgearr5) then begin
	yr = [rmin45,rmax45] + (rmax45-rmin45)*[-.1,.1]
        ;yr = [118964.5, 118968.5]
	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[2], xtickn=xtn
	
	if keyword_set(redgearr4) then begin
		oplot,redgearr4[rind,*],redgearr4[rind+1,*],color=clr(0)
                mean4=118966.5 ;Jeffreys Gap outer edge radius
                mean4=redgearr4[rind+1,*]*0+mean4
                oplot, xr, mean4, color=clr(2)
	endif

	if keyword_set(redgearr5) then begin
		oplot,redgearr5[rind,*]+dt5,redgearr5[rind+1,*],color=clr(1)
                mean4=118966.5 ;Jeffreys Gap outer edge radius
                mean4=redgearr5[rind+1,*]*0+mean4
                oplot, xr, mean4, color=clr(2)
	endif
endif

if keyword_set(redgearr2) || keyword_set(redgearr3) then begin
	yr = [rmin23,rmax23] + (rmax23-rmin23)*[-.1,.1]
        ;yr = [118626.2, 118630.2]
	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[1], xtickn=xtn

	
	if keyword_set(redgearr2) then begin
		oplot,redgearr2[rind,*],redgearr2[rind+1,*],color=clr(0)
                mean2=118628.2 ;Russell Gap outer edge radius
                mean2=redgearr2[rind+1,*]*0+mean2
                oplot, xr, mean2, color=clr(2)

	endif

	if keyword_set(redgearr3) then begin
		oplot,redgearr3[rind,*]+dt3,redgearr3[rind+1,*],color=clr(1)
                mean2=118628.2 ;Russell Gap outer edge radius
                mean2=redgearr3[rind+1,*]*0+mean2
                oplot, xr, mean2, color=clr(2)
	endif
endif

;  This is a hack and will not work when Encke Inner is not the last one plotted
xtn = '' 
xtit = 'Co-Rotating Longitude (!Uo!N)'

if keyword_set(redgearr0) || keyword_set(redgearr1) then begin
	yr = [rmin01,rmax01] + (rmax01-rmin01)*[-.1,.1]
        ;yr = [118281.4, 118286.4]
	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[0], xtickn=xtn
	
	if keyword_set(redgearr0) then begin
		oplot,redgearr0[rind,*],redgearr0[rind+1,*],color=clr(0)
                mean0=118283.4 ;Herschel Gap outer edge radius
                mean0=redgearr0[rind+1,*]*0+mean0
                oplot, xr, mean0, color=clr(2)
                oplot, xr, mean0 - 1, color=clr(0)
        endif

	if keyword_set(redgearr1) then begin
		oplot,redgearr1[rind,*]+dt1,redgearr1[rind+1,*],color=clr(1)
                mean0=118283.4 ;Herschel Gap outer edge radius
                mean0=redgearr1[rind+1,*]*0+mean0
                oplot, xr, mean0, color=clr(2)
                oplot, xr, mean0 - 1, color=clr(0)
	endif
endif

mainmenu:
reply1 = ''
while reply1 eq '' do begin
  ;print, 'dr = '+strtrim(dr,2)+', dl = '+strtrim(dl,2)+', dtwist = '+strtrim(dtwist,2)+', dt1 = 'strtrim(dt1,2)+', dt3 = 'strtrim(dt3,2)+', dt5 = 'strtrim(dt5,2)+', dt7 = 'strtrim(dt7,2)+'
  print, 'dr, dl, dtwist, dt1, dt3, dt5, dt7, det'
  print, dr, dl, dtwist, dt1, dt3, dt5, dt7, det
  print, 'ADJUST [r]adius, [l]ongitude, t[w]ist, various [t]imes, [e]phemeris time, return to [o]riginal pointing,'
  print, '[a]dd another image, [c]reate PS image, [z] save global .dat file, [h]elp, '
  ;print, '(a/r/l/w/t/o/c/z/q/h)'
  read, reply1
;if imagenum lt 63 then reply1='a' else read, reply1 ;You can
;un-comment out this line to allow you to automatically add edge
;profiles. Can change things under 'a' to easily set everything at one
;value or original values
  case reply1 of
    'r': type = [ 'radial', 'pixels' ]
    'l': type = [ 'longitudinal', 'pixels' ]
    'w': type = [ 'twist', 'milliradians' ]
    't': type = [ 'time', 'degrees of longitude' ]
    'e': begin ;This doesn't actually really work, because et is imported in a different routine
               ; See explanation in linsam_redge_encke_bmb.pro
        bb=0
        goto, retry
    end
    'a': begin
        bb=0

         if keyword_set(Herschel) then begin
		 Hnew=0
	 endif
	
	 if keyword_set(Russell) then begin
		 Snew=0
	 endif

	 if keyword_set(Jeffreys) then begin
		 Jnew=0
	 endif

	 if keyword_set(Kuiper) then begin
		 Unew=0
	 endif
         ;drarrayjeff[imagenum]=dr
         ;dtwistarrayjeff[imagenum]=dtwist
             ;dr=0 ;this is just there so that I can quickly reset all values that might have been nonzero
             ;dtwist=0
             ;dl=0
             ;dtwistarray[imagenum]=dtwist ;These
                                ;can allow you to keep track of
                                ;different values easily
             ;dlarray[imagenum]=dl
             ;drarray[imagenum]=dr
             ;el=(2*!pi/360)*(minl-min(redgearr1[rind,*]))
             ;eltotal=(2*!pi/360)*(224.18415-min(redgearr1[rind,*]))
             ;tbar=.0000489
             ;meantbar=.0000709634
             ;mod_dr[imagenum]=meantbar*eltotal
             ;delr[imagenum]=el*dtwist
             ;if imagenum gt 0 then begin
             ;    eltotal=(2*!pi/360)*(224.18415-min(redgearr0[rind,*]))
             ;    mod_dr[imagenum]=meantbar*eltotal
             ;    meanl[imagenum]=min(redgearr0[rind,*])
             ;delr[imagenum]=delr[imagenum-1]+el*dtwist
             ;tbar[imagenum]=delr[imagenum]/eltotal
         ;endif

             ;minl=min(redgearr1[rind,*])
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
        ;dt1 = .007245778*(_et[0]-_et[imagenum]) ;motion due to Pan
        dt1=0
        dt3 = 0
        ;dt3 = .007245778*(_et[0]-_et[imagenum]) ;motion due to Pan
        dt5 = 0
        dt7 = 0
        dtwist = 0
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
  'e': _det = reply2 ;not sure if et is in seconds or a different measure
  't': begin
      reply4 = ''
      while reply4 eq '' do begin
          ;print, 'The following are values for dt1, dt3, dt5, dt7:'
          ;print, dt1, dt3, dt5, dt7
                                ;This part of the code was never
                                ;modified to accomodate the edges in
                                ;the Cassini division
          print, 'ADJUST time for [1]Encke Inner, [3]Encke Outer, [5]Keeler Inner, '
          print, '[7]Keeler Outer, [9]Outer Edge of A Ring, or [q]uit.'
          ;print, '(1/3/5/7/9/q)'
          read, reply4
            case reply4 of
              '1': dt1 = dt1 + reply2
              '3': dt3 = dt3 + reply2
              '5': dt5 = dt5 + reply2
              '7': dt7 = dt7 + reply2
              'q': goto, quit
              else: reply4 = ''
          endcase
      endwhile
  end
endcase
dr = dr + _dr
dl = dl + _dl
dtwist = dtwist + _dtwist
det = det + _det
;dtwist = 0 ;DDA change

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
  __et=_et[imagenum] + det
  redge_recalc = linsam_to_redge_encke_bmb(cmat,redgearr1[0:1,*],redgearr1[2:3,*],redgearr1[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  redgearr1[10:11,*] = redge_recalc
  redgearr1[12:13,*] = redge2_sigma
endif

if keyword_set(redgearr3) then begin
  __et=_et[imagenum] + det
  redge_recalc = linsam_to_redge_encke_bmb(cmat,redgearr3[0:1,*],redgearr3[2:3,*],redgearr3[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  redgearr3[10:11,*] = redge_recalc
  redgearr3[12:13,*] = redge2_sigma 
endif

if keyword_set(redgearr5) then begin
  __et=_et[imagenum] + det
  redge_recalc = linsam_to_redge_encke_bmb(cmat,redgearr5[0:1,*],redgearr5[2:3,*],redgearr5[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  redgearr5[10:11,*] = redge_recalc
  redgearr5[12:13,*] = redge2_sigma 
endif

if keyword_set(redgearr7) then begin
  __et=_et[imagenum] + det
  redge_recalc = linsam_to_redge_encke_bmb(cmat,redgearr7[0:1,*],redgearr7[2:3,*],redgearr7[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  redgearr7[10:11,*] = redge_recalc
  redgearr7[12:13,*] = redge2_sigma 
endif

goto, nextplot

moveplotandexit:


postscript:

if keyword_set(redgearr1) then redgearr1(rind,*) = redgearr1(rind,*)+dt1
if keyword_set(redgearr3) then redgearr3(rind,*) = redgearr3(rind,*)+dt3
if keyword_set(redgearr5) then redgearr5(rind,*) = redgearr5(rind,*)+dt5
if keyword_set(redgearr7) then redgearr7(rind,*) = redgearr7(rind,*)+dt7

if keyword_set(redgearr1) then redgearr0 = [[redgearr0],[redgearr1]]
if keyword_set(redgearr3) then redgearr2 = [[redgearr2],[redgearr3]]
if keyword_set(redgearr5) then redgearr4 = [[redgearr4],[redgearr5]]
if keyword_set(redgearr7) then redgearr6 = [[redgearr6],[redgearr7]]

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

lzr
@plot_prepare

; X Limits for Inner and Outer Edge (constant for both)
xtn = replicate(' ',20) 
xtit = ' '
;xtit = 'Co-Rotating Longitude (!Uo!N)'
ytit = ['Herschel Gap Outer Edge' ,'Russell Gap Outer Edge','Jeffreys Gap Outer Edge','Kuiper Gap Outer Edge']

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

sat=635
@get_sat_coords
moonlong=0;  171.3 ;sat_polar(1)

xr = [lmin,lmax] + (lmax-lmin)*[-.1,.1] - moonlong

if keyword_set(redgearr6) then begin
	redgearr6(rind,*) = redgearr6(rind,*)-moonlong
	yr = [meadrad6-3*SDmax, meadrad6+3*SDmax]

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[3], xtickn=xtn
	oplot,redgearr6[rind,*],redgearr6[rind+1,*]
  	openw,1,'KuiperGapOuter.dat'
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
  	openw,1,'JeffreysGapOuter.dat'
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
  	openw,1,'RussellGapOuter.dat'
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
  	openw,1,'HerschelGapOuter.dat'
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


Hnew=1
Snew=1
Jnew=1
Unew=1


for i=firstpic,lastpic do begin
    if (i eq 26 ) then i = i+2
    imageNEW=filenames(i)
    periodNEW= rstrpos(imageNEW,'.')
    stemNEW  = strmid(imageNEW,0,periodNEW)
    files = file_search(stemNEW+'.edge_linsam*')
    if( (size(files))(0) eq 0) then stop
    nedges = (size(files))(1);
    titles = strmid(files,29,2)
    Herschel = max(titles eq 'H')
    Russell= max(titles eq 'S')
    Jeffreys  = max(titles eq 'J')
    Kuiper = max(titles eq 'U')

;------------------ Import New Edges ----------------------------;

	if keyword_set(Herschel) then begin
		restore, stemNEW+'.edge_linsamH'
                 __et=_et[imagenum]+det
		redge1 = linsam_to_redge_encke_bmb( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
		redgearr1 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	endif

	if keyword_set(Russell) then begin
		restore, stemNEW+'.edge_linsamS'
                __et=_et[imagenum]+det
		redge1 = linsam_to_redge_encke_bmb( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
		redgearr3 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	endif

	if keyword_set(Jeffreys) then begin
		restore, stemNEW+'.edge_linsamJ'
                __et=_et[imagenum]+det
		redge1 = linsam_to_redge_encke_bmb( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
		redgearr5 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
	endif

	if keyword_set(Kuiper) then begin
		restore, stemNEW+'.edge_linsamU'
                __et=_et[imagenum]+det
		redge1 = linsam_to_redge_encke_bmb( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=stemNEW)
		redgearr7 = [redge_linsam,redge_sigma_linsam,redge_dsigma_linsam,redge1,redge1_sigma,redge1,redge1_sigma,imagenum*(1+0*redge_linsam)]
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

;-------------------------------------------------------------------;


    	restore, imageNEW+'_bsa'
        _dr = dr
    	;_dt = dt
        _dt1 = dt1
        _dt3 = dt3
        _dt5 = dt5
        _dt7 = dt7
    	_dl = dl
        _dtwist = dtwist
    	arr_len = 1
    	noplot = 1
    	@draw_arrows
    	x_move = (arr_sat_coords[0,1]-arr_sat_coords[1,1])*_dr + (arr_orb_coords[1,1]-arr_orb_coords[0,1])*_dl
    	y_move = (arr_sat_coords[0,0]-arr_sat_coords[1,0])*_dr + (arr_orb_coords[1,0]-arr_orb_coords[0,0])*_dl
    	@move_bypixel

	if keyword_set(redgearr1) then begin
                __et=_et[imagenum]+det ;This doesn't actually work, unfortunately. See explanation in linsam_to_redge_encke_bmb.pro
  		redge_recalc = linsam_to_redge_encke_bmb(cmat,redgearr1[0:1,*],redgearr1[2:3,*],redgearr1[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  		redgearr1[10:11,*] = redge_recalc
		redgearr1[12:13,*] = redge2_sigma
	endif
	if keyword_set(redgearr3) then begin
                __et=_et[imagenum]+det
  		redge_recalc = linsam_to_redge_encke_bmb(cmat,redgearr3[0:1,*],redgearr3[2:3,*],redgearr3[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  		redgearr3[10:11,*] = redge_recalc
  		redgearr3[12:13,*] = redge2_sigma 
	endif
	if keyword_set(redgearr5) then begin
                __et=_et[imagenum]+det
  		redge_recalc = linsam_to_redge_encke_bmb(cmat,redgearr5[0:1,*],redgearr5[2:3,*],redgearr5[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  		redgearr5[10:11,*] = redge_recalc
  		redgearr5[12:13,*] = redge2_sigma 
	endif
	if keyword_set(redgearr7) then begin
                __et=_et[imagenum]+det
  		redge_recalc = linsam_to_redge_encke_bmb(cmat,redgearr7[0:1,*],redgearr7[2:3,*],redgearr7[4:5,*],redge1_sigma=redge2_sigma,filestem=stemNEW)
  		redgearr7[10:11,*] = redge_recalc
  		redgearr7[12:13,*] = redge2_sigma 
	endif

if not keyword_set(Hnew) then begin
  if keyword_set(Herschel) then   redgearr1(rind,*) = redgearr1(rind,*)+dt1
  if keyword_set(Herschel) then   redgearr0 = [[redgearr0],[redgearr1]]
  if keyword_set(Herschel) then   redgearr0 = redgearr0[*,sort(redgearr0(rind,*))]
endif

if not keyword_set(Snew) then begin
  if keyword_set(Russell) then  redgearr3(rind,*) = redgearr3(rind,*)+dt3
  if keyword_set(Russell) then  redgearr2 = [[redgearr2],[redgearr3]]
  if keyword_set(Russell) then  redgearr2 = redgearr2[*,sort(redgearr2(rind,*))]
endif

if not keyword_set(Jnew) then begin
  if keyword_set(Jeffreys) then  redgearr5(rind,*) = redgearr5(rind,*)+dt5
  if keyword_set(Jeffreys) then  redgearr4 = [[redgearr4],[redgearr5]]
  if keyword_set(Jeffreys) then  redgearr4 = redgearr4[*,sort(redgearr4(rind,*))]
endif

if not keyword_set(Unew) then begin
  if keyword_set(Kuiper) then redgearr7(rind,*) = redgearr7(rind,*)+dt7
  if keyword_set(Kuiper) then redgearr6 = [[redgearr6],[redgearr7]]
  if keyword_set(Kuiper) then redgearr6 = redgearr6[*,sort(redgearr6(rind,*))]
endif

if keyword_set(Hnew) then begin
  if keyword_set(Herschel) then   redgearr0 = redgearr1
  if keyword_set(Herschel) then   redgearr0(rind,*) = redgearr0(rind,*)+dt1
  if keyword_set(Herschel) then Hnew=0
endif 

if keyword_set(Snew) then begin
  if keyword_set(Russell) then  redgearr2 = redgearr3
  if keyword_set(Russell) then  redgearr2(rind,*) = redgearr2(rind,*)+dt3
  if keyword_set(Russell) then Snew=0
endif

if keyword_set(Jnew) then begin
  if keyword_set(Jeffreys) then  redgearr4 = redgearr5
  if keyword_set(Jeffreys) then  redgearr4(rind,*) = redgearr4(rind,*)+dt5
  if keyword_set(Jeffreys) then Jnew=0
endif

if keyword_set(Unew) then begin
  if keyword_set(Kuiper) then redgearr6 = redgearr7
  if keyword_set(Kuiper) then redgearr6(rind,*) = redgearr6(rind,*)+dt7
  if keyword_set(Kuiper) then Unew=0
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

lzr
@plot_prepare


xtn = replicate(' ',20) 
xtit = ' '
;xtit = 'Co-Rotating Longitude (!Uo!N)'
ytit = ['Herschel Gap Outer Edge' ,'Russell Gap Outer Edge','Jeffreys Gap Outer Edge','Kuiper Gap Outer Edge']

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

moonlong = 0

xr = [lmin,lmax] + (lmax-lmin)*[-.1,.1]


if keyword_set(redgearr6) then begin
	redgearr4(rind,*) = redgearr4(rind,*)-moonlong
	yr = [meadrad6-3*SDmax, meadrad6+3*SDmax]

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[3], xtickn=xtn
	oplot,redgearr6[rind,*],redgearr6[rind+1,*]
  	openw,1,'KuiperGapOuter.dat'
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
  	openw,1,'JeffreysGapOuter.dat'
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
  	openw,1,'RussellGapOuter.dat'
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
  	openw,1,'HerschelGapOuter.dat'
    	for j=0l,(size(redgearr0))(2)-1 do begin
       		printf,1,redgearr0(rind,j),redgearr0(rind+1,j),redgearr0(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif

;writedata = ''
;read,writedata,prompt='Write .dat files? : [y/n]'

;B. Byington doesn't know what all the below stuff was - it was
;                                                        commented out
;                                                        in the
;                                                        original code
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
