;; -------------------- bootstrap_redge_encke.pro ------------------ ;;
;; DEPENDENCIES:   .edge files for images in sequence
;; 
;; SUMMARY:	  This the the MAIN code.  It takes the individual edge profiles
;;		  which must be stored in the format:
;;			image_name.linsamEI  (for Encke Inner)
;;			image_name.linsamEO  (for Encke Outer)
;;			image_name.linsamKI  (for Keeler Inner)
;;			image_name.linsamKO  (for Keeler Outer)
;;			image_name.linsamR   (for outer edge of the A ring)
;; This code will load these edge profiles, and allow the user to move them 
;; according in the longitudinal, radial, and time directions. Additional 
;; edge profiles can be added through the interactive menu, allowing for global 
;; maps to be created.
;; SOME MODIFICATION OF THIS CODE MAY BE REQUIRED TO EITHER ENABLE/DISABLE 
;; ASSUMED OR FIXED PATTERN SPEEDS.  The "dt" variable can either be fixed 
;; for every image,  or it can be a free parameter.
;; CREATED BY:  P.TORREY 5/15/08
;; LAST MODIFIED:  M. Tiscareno 7/2009, B. BYINGTON 7/2009

; Window dimensions
winx = 1200 ;1700 ;1800
winy = 800 ;1050 ;1150

firstimage = 1000
lastimage  = 0
rind = 10
imagenum = 0

redgearr0=0   ; Previous EI arrays
redgearr1=0   ; Current EI array
redgearr2=0   ; Previous EO arrays
redgearr3=0   ; Current EO array
redgearr4=0   ; Previous KI arrays
redgearr5=0   ; Current KI array
redgearr6=0   ; Previous KO arrays
redgearr7=0   ; Current KO array
redgearr8=0   ; Previous R arrays
redgearr9=0   ; Current R array

;EIcount=0
;EOcount=0
;KIcount=0
;KOcount=0
;Rcount=0
;ALLcount = [ EIcount, EOcount, KIcount, KOcount, Rcount ]
ALLcount = [ 0, 0, 0, 0, 0 ]
ALLnames = [ 'EnckeGapInner', 'EnckeGapOuter', 'KeelerGapInner', $
             'KeelerGapOuter', 'ARingEdge' ]
ALLabbrev = [ 'EI', 'EO', 'KI', 'KO', 'R' ]
nALL = n_elements(ALLnames)

;EInew=1
;EOnew=1
;KInew=1
;KOnew=1
;Rnew=1
;ALLnew = [ EInew, EOnew, KInew, KOnew, Rnew ]
ALLnew = [ 1, 1, 1, 1, 1 ]

;;;Introducing for now 10 dt and _dt variables to account for varying
;;;pattern speeds. Should only need 5 of them (most likely only the
;;;current arrays) but will keep all for now until that is figured
;;;out. Need to figure out where dt in the entire code will need to be
;;;changed, and may want to introduce simple dt arrays.
dr=0.
dl=0.
;;;dt=0
;dt0=0     ; Previous EI arrays time offset
dt1=0.     ; Current EI array time offset
;dt2=0     ; Previous EO arrays time offset
dt3=0.     ; Current EO array time offset
;dt4=0     ; Previous KI arrays time offset
dt5=0.     ; Current KI array time offset
;dt6=0     ; Previous KO arrays time offset
dt7=0.     ; Current KO array time offset
;dt8=0     ; Previous R arrays time offset
dt9=0.     ; Current R array time offset
;ALLdt = [ dt0, dt1, dt2, dt3, dt4, dt5, dt6, dt7, dt8, dt9 ]
ALLzero = [ 0., 0., 0., 0., 0. ]
ALLdt = ALLzero
_dr=0.
_dl=0.
;;;dt=0
;_dt0=0
_dt1=0.
;_dt2=0
_dt3=0.
;_dt4=0
_dt5=0.
;_dt6=0
_dt7=0.
;_dt8=0
_dt9=0.
;ALL_dt = [ _dt0, _dt1, _dt2, _dt3, _dt4, _dt5, _dt6, _dt7, _dt8, _dt9 ]
ALL_dt = ALLzero

@bootstrap_initialize
help,polera,poledec
;------------------------------------------------------------------------------;

goto, mainmenu

add:

if total(ALLnew) eq nALL or keyword_set(chooseimage) then begin
  imageNEW=''
  read,imageNEW,prompt='Starting Image Number : '
  imageNEW = fix(imageNEW)
  imagenum = fix(imageNEW)
  if (imagenum ge lastimage)  then lastimage = imagenum
  if (imagenum le firstimage) then firstimage= imagenum
endif else begin
  if (imagenum eq 25) then imagenum = imagenum+2
  imagenum = imagenum+1
  if (imagenum ge lastimage)  then lastimage = imagenum
  if (imagenum le firstimage) then firstimage= imagenum
  imageNEW = imagenum
  print,'Next Image:  '+strtrim(imagenum,2)
end  

imageNEW = filenames(imageNEW)
print, imageNEW
globalmap1:
periodNEW= rstrpos(imageNEW,'.')
stemNEW  = strmid(imageNEW,0,periodNEW)
files = file_search(stemNEW+'.edge_linsam*')

if( (size(files))(0) eq 0) then stop
nedges = (size(files))(1);
titles = strmid(files,29,2)
;KeelerInner = max(titles eq 'KI')
;KeelerOuter= max(titles eq 'KO')
;EnckeInner  = max(titles eq 'EI')
;EnckeOuter = max(titles eq 'EO')
;Roche       = max(titles eq 'R')

if not keyword_set(gmap) then begin
  _dr = 0
  ;;;_dt = 0
  ALL_dt = ALLzero
  _dl = 0
  dr=0
  dl=0
  ;;;dt=0
  ALLdt = ALLzero
endif

;------------------ Import New Edges ----------------------------;

for mm=0,nALL-1 do begin
  if (where( titles eq ALLabbrev[mm] ))[0] ne -1 then begin
    restore, stemNEW+'.edge_linsam'+ALLabbrev[mm]
    redge1 = linsam_to_redge_encke( redge_cmat, redge_linsam, $
                                    redge_sigma_linsam, redge_dsigma_linsam, $
                                    redge1_sigma=redge1_sigma, filestem=stemNEW)
    redgearr = [ redge_linsam, redge_sigma_linsam, redge_dsigma_linsam, $
                 redge1, redge1_sigma, redge1, redge1_sigma, $
                 imagenum*(1+0*redge_linsam) ]
    ;redgearr5 = redgearr
    gg = execute( 'redgearr'+strtrim(2*mm+1,2)+' = redgearr' )
    ;KIcount = KIcount+1
    ;;gg = execute( ALLabbrev[mm]+'count = '+ALLabbrev[mm]+'count+1' )
    ALLcount[mm] = ALLcount[mm] + 1
  endif else begin
    ;redgearr5 = 0
    gg = execute( 'redgearr'+strtrim(2*mm+1,2)+' = 0' )
  endelse
endfor

cmat = REDGE_CMAT
if not keyword_set(gmap) then cmat_orig = cmat

; -------------------- Filter New Edges -----------------------------;
filtersize= 2.5
for mm=0,nALL-1 do begin
  ;if keyword_set(redgearr5) then $
  ;                      redgearr5 = medianfilter(redgearr5,filtersize,rind)
  gg = execute( 'if keyword_set(redgearr'+strtrim(2*mm+1,2)+') then redgearr'+$
                strtrim(2*mm+1,2)+' = medianfilter(redgearr'+strtrim(2*mm+1,2)+$
                ',filtersize,rind)' )
endfor

;--------------------- Adjust New Edges -----------------------------;

if keyword_set(findfile(imageNEW+'_bsa')) then begin
  restore, imageNEW+'_bsa'
  print,'_bsa file found and loading...'
  _dr = dr
  ;;;_dt = dt
  ALL_dt = ALLdt
  _dl = dl
  goto, adjust
endif

nextplot:
if !d.name eq 'X' then window, xs=winx, ys=winy

;;;_dt = 0
;;;_dt = dt
   ALL_dt = ALLzero
   ALL_dt = ALLdt

;   This handles the graph parameters 
!p.charsize=1.5
!p.multi = [0,1,nedges] 
!x.margin = [13,3]
!y.margin = [0,0]
!y.omargin = [4,2]

clr = [ ctred(), ctwhite(), ctgreen() ]

;---------------- GRAPHING LOOP -------------------;

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

for mm=0,nALL*2-1 do begin
  ;if keyword_set(redgearr4) then yes=1 else yes=0
  gg = execute( 'if keyword_set(redgearr'+strtrim(mm,2)+$
                ') then yes=1 else yes=0' )
  if keyword_set(yes) then begin
    ;lmin = min( [ lmin, min(redgearr4[rind,*]) ] )
    gg = execute( 'lmin = min( [ lmin, min(redgearr'+strtrim(mm,2)+'[rind,*]) ] )' )
    ;lmax = max( [ lmax, max(redgearr4[rind,*]) ] )
    gg = execute( 'lmax = max( [ lmax, max(redgearr'+strtrim(mm,2)+'[rind,*]) ] )' )
    ;; Note that we use rmin45 for mm=4 and for mm=5
    ALLind = strtrim(fix(mm/2)*2,2) + strtrim(fix(mm/2)*2+1,2)
    ;rmin45 = min( [rmin45, min(redgearr4[rind+1,*]) ] )
    gg = execute( 'rmin'+ALLind+' = min( [rmin'+ALLind+', min(redgearr'+$
                  strtrim(mm,2)+'[rind+1,*]) ] )' )
    ;rmax45 = max( [rmax45, max(redgearr4[rind+1,*]) ] )
    gg = execute( 'rmax'+ALLind+' = max( [rmax'+ALLind+', max(redgearr'+$
                  strtrim(mm,2)+'[rind+1,*]) ] )' )
  endif
endfor

xr = [lmin,lmax] + (lmax-lmin)*[-.1,.1]

for mm=nALL-1,0,-1 do begin
  ;if keyword_set(redgearr4) || keyword_set(redgearr5) then yes=1 else yes=0
  gg = execute( 'if keyword_set(redgearr'+strtrim(2*mm,2)+$
                ') || keyword_set(redgearr'+strtrim(2*mm+1,2)+$
                ') then yes=1 else yes=0' )
  if keyword_set(yes) then begin
    if mm eq min(where(ALLcount)) then begin
      xtn = '' 
      xtit = 'Co-Rotating Longitude (!Uo!N)'
    endif else begin
      xtn = replicate(' ',20) 
      xtit = ' '
    endelse
    ALLind = strtrim(fix(mm)*2,2) + strtrim(fix(mm)*2+1,2)
    ;yr = [rmin45,rmax45] + (rmax45-rmin45)*[-.1,.1]
    gg = execute( 'yr = [rmin'+ALLind+',rmax'+ALLind+'] + (rmax'+ALLind+$
                  '-rmin'+ALLind+')*[-.1,.1]' )
    plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[2], xtickn=xtn
    ;if keyword_set(redgearr4) then $
    ;             oplot,redgearr4[rind,*],redgearr4[rind+1,*],color=clr[0]
    ;if keyword_set(redgearr5) then $
    ;             oplot,redgearr5[rind,*]+dt,redgearr5[rind+1,*],color=clr[1]
    ;;;for nn=0,1 do gg = execute( 'if keyword_set(redgearr'+strtrim(2*mm+nn,2)+$
                                ;;;') then oplot,redgearr'+strtrim(2*mm+nn,2)+$
                                ;;;'[rind,*]+dt*'+strtrim(nn,2)+',redgearr'+$
                                ;;;strtrim(2*mm+nn,2)+'[rind+1,*],color=clr['+$
                                ;;;strtrim(nn,2)+']' )

    for nn=0,1 do gg = execute( 'if keyword_set(redgearr'+strtrim(2*mm+nn,2)+$
                                ') then oplot,redgearr'+strtrim(2*mm+nn,2)+$
                                '[rind,*]+dt'+strtrim(2*mm+nn,2)+'*'+strtrim(nn,2)+',redgearr'+$
                                strtrim(2*mm+nn,2)+'[rind+1,*],color=clr['+$
                                strtrim(nn,2)+']' )
  endif
endfor

mainmenu:
reply1 = ''
while reply1 eq '' do begin
  print, 'dr = '+strtrim(dr,2)+', dl = '+strtrim(dl,2)+', ;;;dt = '+strtrim(dt,2)
   ;;;for mm=0,nALL-1 do gg = execute( print, 'dt"+strtrim(2*mm+1)+" = "+strtrim(dt"+strtrim(2*mm+1)+",2)')
  print, 'Adjust [r]adius, [l]ongitude, [t]ime, '
  print, '[a]dd another image, [c]reate PS image, [z] save global .dat file, '
  print, '(a/r/l/t/c/z/q)'
  read, reply1
  case reply1 of
    'r': type = [ 'radial', 'pixels' ]
    'l': type = [ 'longitudinal', 'pixels' ]
    't': type = [ 'time', 'degrees of longitude' ]
    'a': begin
         for j=0,n_elements(titles)-1 do begin
           ALLnew[where(titles[j] eq ALLabbrev)] = 0
         endfor 

         foo = where( ALLcount ge 2, count )
         if count gt 0 then begin
           sfile = imageNEW+'_bsa'
           if keyword_set(findfile(sfile)) then begin
             ;;;if ( dr ne _dr or dl ne _dl or dt ne _dt ) then begin
             if ( dr ne _dr or dl ne _dl or ALLdt ne ALL_dt ) then begin
               reply3 = ''
               while reply3 eq '' do begin
                 print, 'Save new values of dr, dl, and dt? (y/n)'
                 read, reply3
                 case reply3 of
                   'y': overwrite = 1
                   'n': overwrite = 0
                   else: reply3 = ''
                 endcase 
               endwhile 
             endif else overwrite = 0
           endif else overwrite = 1
         endif

         foo = where( ALLcount ge 2, count )
         if count gt 0 then for m=0,count-1 do begin
           mm = foo[m]
           if (where( titles eq ALLabbrev[mm] ))[0] ne -1 then begin
             ;redgearr5[rind,*] = redgearr5[rind,*]+dt
             ;;;gg = execute( 'redgearr'+strtrim(2*mm+1,2)+'[rind,*] = redgearr'+$
                           ;;;strtrim(2*mm+1,2)+'[rind,*]+dt' )
             gg = execute( 'redgearr'+strtrim(2*mm+1,2)+'[rind,*] = redgearr'+$
                           strtrim(2*mm+1,2)+'[rind,*]+dt+strtrim(2*mm+1,2)' )
             ;redgearr4 = [[redgearr4],[redgearr5]]
             gg = execute( 'redgearr'+strtrim(2*mm,2)+' = [[redgearr'+$
                           strtrim(2*mm,2)+'],[redgearr'+strtrim(2*mm+1,2)+$
                           ']]' )
             ;redgearr4 = redgearr4[*,sort(redgearr4[rind,*])]
             gg = execute( 'redgearr'+strtrim(2*mm,2)+' = redgearr'+$
                           strtrim(2*mm,2)+'[*,sort(redgearr'+strtrim(2*mm,2)+$
                           '[rind,*])]' )
           endif 
           ;;;if keyword_set(overwrite) then save, dr, dl, dt, filename=sfile
           if keyword_set(overwrite) then save, dr, dl, ALLdt, filename=sfile
         endfor

         foo = where( ALLcount eq 1, count )
         if count gt 0 then for m=0,count-1 do begin
           mm = foo[m]
           if (where( titles eq ALLabbrev[mm] ))[0] ne -1 then begin
             ;redgearr4 = redgearr5
             gg = execute( 'redgearr'+strtrim(2*mm,2)+' = redgearr'+$
                           strtrim(2*mm+1,2) )
             ;redgearr4[rind,*] = redgearr4[rind,*]+dt
             ;;;gg = execute( 'redgearr'+strtrim(2*mm,2)+'[rind,*] = redgearr'+$
                           ;;;strtrim(2*mm,2)+'[rind,*]+dt' )
             gg = execute( 'redgearr'+strtrim(2*mm,2)+'[rind,*] = redgearr'+$
                           strtrim(2*mm,2)+'[rind,*]+dt+strtrim(2*mm+1,2)+' )
           endif 
           ;;;if keyword_set(overwrite) then save, dr, dl, dt, filename=sfile
           if keyword_set(overwrite) then save, dr, dl, ALLdt, filename=sfile
         endfor

         goto, add
    end
    'c': goto, postscript
    'q': goto, quit
    'z': goto, globalmap
    else: reply1 = ''
  endcase
endwhile

; You get here if you have chosen to "Adjust [r]adius, [l]ongitude, [t]ime"
reply2 = ''
print, 'Enter '+type[0]+' offset in '+type[1]+' (or "q" to quit).'
read, reply2
if reply2 eq 'q' then goto, quit

reply2 = float(reply2)

_dr = 0.
_dl = 0.
_dt1 = 0.
_dt3 = 0.
_dt5 = 0.
_dt7 = 0.
_dt9 = 0.
case reply1 of
  'r': _dr = reply2
  'l': _dl = reply2
  ;;;'t': dt = dt + reply2
  't': begin
         reply4 = ''
         while reply4 eq '' do begin
            ;for mm=0,nALL-1 do gg = execute (print, 'dt"+strtrim(2*mm+1)+" = "+strtrim(dt"+strtrim(2*mm+1)+",2)')
            ;endfor
            print, dt1 = '+strtrim(dt1,2)+', dt3 = '+strtrim(dt3,2)+', dt5 = '+strtrim(dt5,2)+', dt7 = '+strtrim(dt7,2)+',dt9 = '+strtrim(dt9,2)+'
            print, 'Adjust time for [1]Encke Inner, [3]Encke Outer, [5]Keeler Inner, '
            print, '[7]Keeler Outer, [9]Outer Edge of A Ring, or [q]uit.'
            print, '(1/3/5/7/9/q)'
            read, reply4
            case reply4 of
              '1': _dt1 = reply2
              '3': _dt3 = reply2
              '5': _dt5 = reply2
              '7': _dt7 = reply2
              '9': _dt9 = reply2
              'q': goto, quit
          endcase
      endwhile
  endcase

;;;For some reason, the following cause 'syntax error' to
;;;appear on screen when running program, and the program will not
;;;quit or stop running as commanded. Investigating. -BB


dr = dr + _dr
dl = dl + _dl
ALLdt = ALLdt + ALL_dt
print, ALLdt
;dt1=(dt1 + _dt1)
;dt3=(dt3 + _dt3)
;dt5=(dt5 + _dt5)
;dt7=(dt7 + _dt7)
;dt9=(dt9 + _dt9)

stop, 'Needs more editing.'
;goto, quit

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

for mm=0,nALL-1 do begin
  ;if keyword_set(redgearr5) then yes=1 else yes=0
  gg = execute( 'if keyword_set(redgearr'+strtrim(2*mm+1,2)+$
                ') then yes=1 else yes=0' )
  if keyword_set(yes) then begin
    ;redge_recalc = linsam_to_redge_encke( cmat, redgearr5[0:1,*], $
    ;                                      redgearr5[2:3,*], redgearr5[4:5,*], $
    ;                                      redge1_sigma=redge2_sigma, $
    ;                                      filestem=stemNEW )
    gg = execute( 'redge_recalc = linsam_to_redge_encke( cmat, redgearr'+$
                  strtrim(2*mm+1,2)+'[0:1,*], redgearr'+strtrim(2*mm+1,2)+$
                  '[2:3,*], redgearr'+strtrim(2*mm+1,2)+$
                  '[4:5,*], redge1_sigma=redge2_sigma, filestem=stemNEW )' )
    ;redgearr5[10:11,*] = redge_recalc
    gg = execute( 'redgearr'+strtrim(2*mm+1,2)+'[10:11,*] = redge_recalc' )
    ;redgearr5[12:13,*] = redge2_sigma 
    gg = execute( 'redgearr'+strtrim(2*mm+1,2)+'[12:13,*] = redge2_sigma' )
  endif
endfor

if keyword_set(gmap) then goto, globalmap2
goto, nextplot

postscript:

for mm=0,nALL-1 do begin
  ;if keyword_set(redgearr5) then yes=1 else yes=0
  gg = execute( 'if keyword_set(redgearr'+strtrim(2*mm+1,2)+$
                ') then yes=1 else yes=0' )
  if keyword_set(yes) then begin
    ;redgearr5[rind,*] = redgearr5[rind,*]+dt
    gg = execute( 'redgearr'+strtrim(2*mm+1,2)+'[rind,*] = redgearr'+$
                  strtrim(2*mm+1,2)+'[rind,*]+dt' )
    ;redgearr4 = [[redgearr4],[redgearr5]]
    gg = execute( 'redgearr'+strtrim(2*mm,2)+' = [[redgearr'+strtrim(2*mm,2)+$
                  '],[redgearr'+strtrim(2*mm+1,2)+']]' )
  endif
endfor

for mm=0,nALL-1 do begin
  ;if keyword_set(redgearr4) then yes=1 else yes=0
  gg = execute( 'if keyword_set(redgearr'+strtrim(2*mm,2)+$
                ') then yes=1 else yes=0' )
  if keyword_set(yes) then begin
    ;redgearr4 = redgearr4[*,sort(redgearr4[rind,*])]
    gg = execute( 'redgearr'+strtrim(2*mm,2)+' = redgearr'+strtrim(2*mm,2)+$
                  '[*,sort(redgearr'+strtrim(2*mm,2)+'[rind,*])]' )
    ;meadrad4 = median(redgearr4[rind+1,*])
    gg = execute( 'meadrad'+strtrim(2*mm,2)+' = median(redgearr'+$
                  strtrim(2*mm,2)+'[rind+1,*])' )
    ;SD4 = stddev(redgearr4[rind+1,*])
    gg = execute( 'SD'+strtrim(2*mm,2)+' = stddev(redgearr'+strtrim(2*mm,2)+$
                  '[rind+1,*])' )
  endif
endfor

lzr
@plot_prepare

lmin = 1000
lmax = -1000
SDmax = 0

for mm=0,nALL-1 do begin
  ;if keyword_set(redgearr4) then yes=1 else yes=0
  gg = execute( 'if keyword_set(redgearr'+strtrim(2*mm,2)+$
                ') then yes=1 else yes=0' )
  if keyword_set(yes) then begin
    ;lmin = min( [ lmin, min(redgearr4[rind,*]) ] )
    gg = execute( 'lmin = min( [ lmin, min(redgearr'+strtrim(2*mm,2)+$
                  '[rind,*]) ] )' )
    ;lmax = max( [ lmax, max(redgearr4[rind,*]) ] )
    gg = execute( 'lmax = max( [ lmax, max(redgearr'+strtrim(2*mm,2)+$
                  '[rind,*]) ] )' )
    ;SDmax = max( [SDmax,SD4] )
    gg = execute( 'SDmax = max( [SDmax,SD'+strtrim(2*mm,2)+'] )' )
  endif
endfor

sat=635
@get_sat_coords
moonlong=0;  171.3 ;sat_polar(1)

xr = [lmin,lmax] + (lmax-lmin)*[-.1,.1] - moonlong

for mm=nALL-1,0,-1 do begin
  ;if keyword_set(redgearr4) then yes=1 else yes=0
  gg = execute( 'if keyword_set(redgearr'+strtrim(2*mm,2)+$
                ') then yes=1 else yes=0' )
  if keyword_set(yes) then begin
    if mm eq min(where(ALLcount)) then begin
      xtn = '' 
      xtit = 'Co-Rotating Longitude (!Uo!N)'
    endif else begin
      xtn = replicate(' ',20) 
      xtit = ' '
    endelse
    ;redgearr4[rind,*] = redgearr4[rind,*]-moonlong
    gg = execute( 'redgearr'+strtrim(2*mm,2)+'[rind,*] = redgearr'+$
                  strtrim(2*mm,2)+'[rind,*]-moonlong' )
    ;yr = [meadrad4-3*SDmax, meadrad4+3*SDmax]
    gg = execute( 'yr = [meadrad'+strtrim(2*mm,2)+'-3*SDmax, meadrad'+$
                  strtrim(2*mm,2)+'+3*SDmax]' )
    plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[mm], xtickn=xtn
    ;oplot,redgearr4[rind,*],redgearr4[rind+1,*]
    gg = execute( 'oplot,redgearr'+strtrim(2*mm,2)+'[rind,*],redgearr'+$
                  strtrim(2*mm,2)+'[rind+1,*]' )
    openw,1,ALLnames[mm]+'.dat'
    ;for j=0l,(size(redgearr4))(2)-1 do printf, 1, redgearr4[rind,j], $
    ;         redgearr4[rind+1,j], redgearr4(15,j), FORMAT="(D11.6,D15.5,I8.3)"
    gg = execute( 'for j=0l,(size(redgearr'+strtrim(2*mm,2)+$
                  '))(2)-1 do printf, 1, redgearr'+strtrim(2*mm,2)+$
                  '[rind,j], redgearr'+strtrim(2*mm,2)+'[rind+1,j], redgearr'+$
                  strtrim(2*mm,2)+'(15,j), FORMAT="(D11.6,D15.5,I8.3)"' )
    close,1
  endif
endfor

clzr

goto, quit

globalmap:

stop, 'From here on, the code still needs work.'
  
;@bootstrap_initialize
gmap = 1

firstpic=''
lastpic =''
read,firstpic,prompt='First Included Picture Number : '
read,lastpic,prompt ='Last Included Picture Number : '
firstpic = fix(firstpic)
lastpic  = fix(lastpic)

;KInew=1
;KOnew=1
;EInew=1
;EOnew=1
;Rnew =1
ALLnew = [ 1, 1, 1, 1, 1 ]

for i=firstpic,lastpic do begin
  if (i eq 26 ) then i = i+2
  imageNEW=filenames(i)
  goto, globalmap1
  globalmap2:
  for mm=0,nALL-1 do if ALLnew[mm] eq 0 then begin
    if (where( titles eq ALLabbrev[mm] ))[0] ne -1 then begin
      ;redgearr5[rind,*] = redgearr5[rind,*]+dt
      gg = execute( 'redgearr'+strtrim(2*mm+1,2)+'[rind,*] = redgearr'+$
                    strtrim(2*mm+1,2)+'[rind,*]+dt' )
      ;redgearr4 = [[redgearr4],[redgearr5]]
      gg = execute( 'redgearr'+strtrim(2*mm,2)+' = [[redgearr'+$
                    strtrim(2*mm,2)+'],[redgearr'+strtrim(2*mm+1,2)+']]' )
      ;redgearr4 = redgearr4[*,sort(redgearr4[rind,*])]
      gg = execute( 'redgearr'+strtrim(2*mm,2)+' = redgearr'+strtrim(2*mm,2)+$
                    '[*,sort(redgearr'+strtrim(2*mm,2)+'[rind,*])]' )
    endif
  endif
  for mm=0,nALL-1 do if ALLnew[mm] eq 1 then begin
    if (where( titles eq ALLabbrev[mm] ))[0] ne -1 then begin
      ;redgearr4 = redgearr5
      gg = execute( 'redgearr'+strtrim(2*mm,2)+' = redgearr'+strtrim(2*mm+1,2) )
      ;redgearr4[rind,*] = redgearr4[rind,*]+dt
      gg = execute( 'redgearr'+strtrim(2*mm,2)+'[rind,*] = redgearr'+$
                    strtrim(2*mm,2)+'[rind,*]+dt' )
      ALLnew[mm] = 0
    endif
  endif
  print,i
endfor

if not keyword_exists(smoothKO) then smoothKO = 1
if keyword_set(smoothKO) then begin
  smoothinglength = 2000
  if keyword_set(redgearr6) then begin
    fix = 136522.0 - smooth( redgearr6[rind+1,*], smoothinglength, $
                             edge_truncate=1 )
    minsize = min( [(size(redgearr4))(2), (size(redgearr6))(2) ]) -1 ;, $
    ;          min( [(size(redgearr0))(2), (size(redgearr2))(2), $
    ;                (size(redgearr4))(2) , (size(redgearr6))(2) , $
    ;                (size(redgearr8))(2) ] )
    fix = fix[0:minsize]
  endif
endif

if keyword_set(redgearr0) then begin
	redgearr0[rind+1,*] = redgearr0[rind+1,*]
	redgearr0 = redgearr0[*,sort(redgearr0[rind,*])]
	meadrad0 = median(redgearr0[rind+1,*])
	SD0 = stddev(redgearr0[rind+1,*])
endif

;;;for mm=0,nALL-1 do
      ;;;if keyword_set(redgearr'+strtrim(2*mm,2)+') then begin
        ;;;gg = execute( redgearr'+strtrim(2*mm,2)+'[rind+1,*] = redgearr'+strtrim(2*mm,2)+'[rind+1,*])
        ;;;gg = execute( redgearr'+strtrim(2*mm,2)+' = redgearr'+strtrim(2*mm,2)+'[*,sort(redgearr'+strtrim(2*mm,2)+'[rind,*])])
        ;;;gg = execute( meadrad'+strtrim(2*mm,2)+' = median(redgearr'+strtrim(2*mm,2)+'[rind+1,*]))
        ;;;gg = execute( SD'+strtrim(2*mm,2)+' = stddev(redgearr'+strtrim(2*mm,2)+'[rind+1,*])
      ;;;endif
;;;endfor


if keyword_set(redgearr2) then begin
	redgearr2[rind+1,*] = redgearr2[rind+1,*]
	redgearr2 = redgearr2[*,sort(redgearr2[rind,*])]
	meadrad2 = median(redgearr2[rind+1,*])
	SD2 = stddev(redgearr2[rind+1,*])
endif

if keyword_set(redgearr4) then begin
	redgearr4 = redgearr4(*,0:minsize)
	redgearr4[rind+1,*] = redgearr4[rind+1,*]+fix
	redgearr4 = redgearr4[*,sort(redgearr4[rind,*])]
	meadrad4 = median(redgearr4[rind+1,*])
	SD4 = stddev(redgearr4[rind+1,*])
endif


if keyword_set(redgearr6) then begin
	redgearr6 = redgearr6(*,0:minsize)
	redgearr6[rind+1,*] = redgearr6[rind+1,*]+fix
	redgearr6 = redgearr6[*,sort(redgearr6[rind,*])]
	meadrad6 = median(redgearr6[rind+1,*])
	SD6 = stddev(redgearr6[rind+1,*])
endif

if keyword_set(redgearr8) then begin
	redgearr8 = redgearr8(*,0:minsize)
	redgearr8[rind+1,*] = redgearr8[rind+1,*]+fix
	redgearr8 = redgearr8[*,sort(redgearr8[rind,*])]
	meadrad8 = median(redgearr8[rind+1,*])
	SD8 = stddev(redgearr8[rind+1,*])
endif

lzr
@plot_prepare

lmin = 1000
lmax = -1000
SDmax = 0

if keyword_set(redgearr0) then begin
	lmin = min( [lmin, min(redgearr0[rind,*]) ] )
	lmax = max( [lmax, max(redgearr0[rind,*]) ] )
	SDmax = max( [SDmax,SD0] )
endif

;;;for mm=0,nALL-1 do
   ;;;if keyword_set(redgearr'+strtrim(2*mm,2)+') then begin 
      ;;;gg = execute( 'lmin = min( [lmin, min(redgearr'+strtrim(2*mm,2)+'[rind,*]) ] )')
      ;;;gg = execute( 'lmax = max( [lmax, max(redgearr'+strtrim(2*mm,2)+'[rind,*]) ] )')
      ;;;gg = execute( 'SDmax = max( [SDmax,SD'+strtrim(2*mm,2)+'] )')
   ;;;endif
;;;endfor

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


if keyword_set(redgearr0) then begin
	redgearr0[rind,*] = redgearr0[rind,*]-moonlong
	yr = [meadrad0-3*SDmax, meadrad0+3*SDmax] 

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[0], xtickn=xtn
	oplot,redgearr0[rind,*],redgearr0[rind+1,*]
  	openw,1,'EnckeGapInner.dat'
    	for j=0l,(size(redgearr0))(2)-1 do begin
       		printf,1,redgearr0[rind,j],redgearr0[rind+1,j],redgearr0(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif

if keyword_set(redgearr2) then begin
	redgearr2[rind,*] = redgearr2[rind,*]-moonlong
	yr = [meadrad2-3*SDmax, meadrad2+3*SDmax]

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[1], xtickn=xtn
	oplot,redgearr2[rind,*],redgearr2[rind+1,*]
  	openw,1,'EnckeGapOuter.dat'
    	for j=0l,(size(redgearr2))(2)-1 do begin
       		printf,1,redgearr2[rind,j],redgearr2[rind+1,j],redgearr2(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif


if keyword_set(redgearr6) then begin
	redgearr4[rind,*] = redgearr4[rind,*]-moonlong
	yr = [meadrad6-3*SDmax, meadrad6+3*SDmax]

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[3], xtickn=xtn
	oplot,redgearr6[rind,*],redgearr6[rind+1,*]
  	openw,1,'KeelerGapOuter.dat'
    	for j=0l,(size(redgearr6))(2)-1 do begin
       		printf,1,redgearr6[rind,j],redgearr6[rind+1,j],redgearr6(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif

;  This is a hack and will not work when the ROUCH gap is not the last one plotted
xtn = '' 
xtit = 'Co-Rotating Longitude (!Uo!N)'

if keyword_set(redgearr4) then begin
	redgearr6[rind,*] = redgearr6[rind,*]-moonlong
	yr = [meadrad4-3*SDmax, meadrad4+3*SDmax]

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[2], xtickn=xtn
	oplot,redgearr4[rind,*],redgearr4[rind+1,*]
  	openw,1,'KeelerGapInner.dat'
    	for j=0l,(size(redgearr4))(2)-1 do begin
       		printf,1,redgearr4[rind,j],redgearr4[rind+1,j],redgearr4(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    	endfor
  	close,1
endif


if keyword_set(redgearr8) then begin
	redgearr8[rind,*] = redgearr8[rind,*]-moonlong
	yr = [meadrad8-3*SDmax, meadrad8+3*SDmax] 

	plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[4], xtickn=xtn
	oplot,redgearr8[rind,*],redgearr8[rind+1,*]
  	openw,1,'AGap.dat'
    	for j=0l,(size(redgearr8))(2)-1 do begin
       		printf,1,redgearr8[rind,j],redgearr8[rind+1,j],redgearr8(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
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
 ;     			printf,1,redgearr0[rind,j],redgearr0[rind+1,j],redgearr0(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
  ;  		endfor
  ;		close,1
;	endif
;
 ; 	if keyword_set(redgearr2) then begin
;		openw,1,dataset+'EO.dat'
;	    	for j=0l,(size(redgearr2))(2)-1 do begin
 ;     			printf,1,redgearr2[rind,j],redgearr2[rind+1,j],redgearr2(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
  ;  		endfor
  ;		close,1
;	endif
;
;
;
 ; 	if keyword_set(redgearr4) then begin
;		openw,1,dataset+'KI.dat'
;	    	for j=0l,(size(redgearr4))(2)-1 do begin
 ;     			printf,1,redgearr4[rind,j],redgearr4[rind+1,j],redgearr4(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
  ;  		endfor
  ;		close,1
;	endif
;
 ; 	if keyword_set(redgearr6) then begin
;		openw,1,dataset+'KO.dat'
;	    	for j=0l,(size(redgearr6))(2)-1 do begin
 ;     			printf,1,redgearr6[rind,j],redgearr6[rind+1,j],redgearr6(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
  ;  		endfor
  ;		close,1
;	endif
;
;
 ; 	if keyword_set(redgearr8) then begin
;		openw,1,dataset+'R.dat'
;	    	for j=0l,(size(redgearr8))(2)-1 do begin
 ;     			printf,1,redgearr8[rind,j],redgearr8[rind+1,j],redgearr8(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
  ;  		endfor
  ;		close,1
;	endif
;
;endif

; This section was previously in a separate file called by "@bootstrap_dat"
writedata = ''
read,writedata,prompt='Write .dat files? : [y/n]'
if (writedata eq 'y' || writedata eq 'Y') then begin
  writedata = ''
  read,writedata,prompt='Base File Name : '
  if keyword_set(redgearr0) then begin

    openw,1,writedata+'EI.dat'
    for j=0l,(size(redgearr0))(2)-1 do begin
      printf,1,redgearr0[rind,j],redgearr0[rind+1,j],$
             redgearr0(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    endfor
    close,1
    
    enckeInnerx = dblarr(1,50000)
    enckeInnery = dblarr(1,50000)
    
    enckeInnerx = interpol(redgearr0[rind,*],50000)
    enckeInnery = interpol(redgearr0[rind+1,*],50000)
    
    enckeInnery = enckeInnery - smooth(enckeInnery,500,edge_truncate=1)
    
  endif
  if keyword_set(redgearr2) then begin

    openw,1,writedata+'EO.dat'
    for j=0l,(size(redgearr2))(2)-1 do begin
      printf,1,redgearr2[rind,j],redgearr2[rind+1,j],$
             redgearr2(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    endfor
    close,1

    enckeOuterx = dblarr(1,50000)
    enckeOutery = dblarr(1,50000)	

    enckeOuterx = interpol(redgearr2[rind,*],50000)
    enckeOutery = interpol(redgearr2[rind+1,*],50000)

    enckeOutery = enckeOutery - smooth(enckeOutery,500,edge_truncate=1)
  endif
  if keyword_set(redgearr4) then begin
    openw,1,writedata+'KI.dat'
    for j=0l,(size(redgearr4))(2)-1 do begin
      printf,1,redgearr4[rind,j],redgearr4[rind+1,j],$
             redgearr4(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    endfor
    close,1
		
    keelerInnerx = dblarr(1,50000)
    keelerInnery = dblarr(1,50000)

    keelerInnerx = interpol(redgearr4[rind,*],50000)
    keelerInnery = interpol(redgearr4[rind+1,*],50000)
  endif
  if keyword_set(redgearr6) then begin
    openw,1,writedata+'KO.dat'
    for j=0l,(size(redgearr6))(2)-1 do begin
      printf,1,redgearr6[rind,j],redgearr6[rind+1,j],$
             redgearr6(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    endfor
    close,1

    keelerOuterx = dblarr(1,10000)
    keelerOutery = dblarr(1,10000)

    keelerOuterx = interpol(redgearr6[rind,*],10000)
    keelerOutery = interpol(redgearr6[rind+1,*],10000)
  endif
  if keyword_set(redgearr8) then begin
    openw,1,writedata+'R.dat'
    for j=0l,(size(redgearr8))(2)-1 do begin
      printf,1,redgearr8[rind,j],redgearr8[rind+1,j],$
             redgearr8(15,j),FORMAT = '(D11.6,D15.5,I8.3)'
    endfor
    close,1
  endif

endif

if keyword_set(keelerInnerx) then begin
  save,keelerInnerx,keelerInnery,keelerOuterx,keelerOutery,$
       filename = writedata+'.sav'
endif
if keyword_set(enckeInnerx) then begin
   save,enckeInnerx,enckeInnery,enckeOuterx,enckeOutery,$
        filename = writedata+'.sav'
endif

clzr

quit:
!x.margin = [10,3]
!y.margin = [4,2]
!y.omargin = [0,0]

end

