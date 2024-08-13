;Troubleshooting program to figure out what the problems are in
;bootstrap_redge_encke.pro, using text from bootstrap_redge_encke.pro
;and bootstrap_redge.pro. 
;This program allows the user to manipulate values of dr, dl, dt (with
;varying pattern speeds) to fit edges together for different gaps,
;saving the values of dr, dl, dt that make the edges fit together in a
;.bsa file.
;Created 7/2009 by B. Byington for figuring out problems with bootstrap_redge_encke.pro
;and learning programming.

;Problems to address:
;Now correctly graphs saved bits when one image is added, but not two.
;Size of graph window should change to accomodate dr, dl, dt changes.
;Test on each rev to make sure other problems don't appear
;Figure out pattern speeds that work for each rev/edge by fitting strings of images with
;good pointers

;if not keyword_set(encke) and not keyword_set(keeler) and not keyword_set(aredge) then encke = 1

restore, 'stretch.sav'
;restore, filefits
device, retain=2

;Window dimensions
winx = 1200
winy = 800

firstimage = 1000
lastimage  = 0
rind = 10
imagenum = 0

;nsf = n_elements(sfile)

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

ALLcount = [ 0, 0, 0, 0, 0 ]
ALLnames = [ 'EnckeGapInner', 'EnckeGapOuter', 'KeelerGapInner', $
             'KeelerGapOuter', 'ARingEdge' ]
ALLabbrev = [ 'EI', 'EO', 'KI', 'KO', 'R' ]
nALL = n_elements(ALLnames)
ALLnew = [ 1, 1, 1, 1, 1 ]

dr=0.
dl=0.
dt1=0.     ; Current EI array time offset
dt3=0.     ; Current EO array time offset
dt5=0.     ; Current KI array time offset
dt7=0.     ; Current KO array time offset
dt9=0.     ; Current R array time offset
_dr=0.
_dl=0.
_dt1=0.
_dt3=0.
_dt5=0.
_dt7=0.
_dt9=0.

@bootstrap_initialize
help,polera,poledec


;-----------'add' goto statement-------------------------------
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
filefits = file_search('*_bsa')

if( (size(files))(0) eq 0) then stop
nedges = (size(files))(1);
titles = strmid(files,29,2)


;if not keyword_set(gmap) then begin
 ; _dr = 0
  ;_dt = 0
  ;_dl = 0
  ;dr=0
  ;dl=0
  ;dt = 0
;endif

;-----------------Import New Edges------------------------

for mm=0,nALL-1 do begin
  if (where( titles eq ALLabbrev[mm] ))[0] ne -1 then begin
    restore, stemNEW+'.edge_linsam'+ALLabbrev[mm]
    redge1 = linsam_to_redge_encke( redge_cmat, redge_linsam, $
                                    redge_sigma_linsam, redge_dsigma_linsam, $
                                    redge1_sigma=redge1_sigma, filestem=stemNEW)
    redgearr = [ redge_linsam, redge_sigma_linsam, redge_dsigma_linsam, $
                 redge1, redge1_sigma, redge1, redge1_sigma, $
                 imagenum*(1+0*redge_linsam) ]
    gg = execute( 'redgearr'+strtrim(2*mm+1,2)+' = redgearr' )
    ALLcount[mm] = ALLcount[mm] + 1
  endif else begin
    gg = execute( 'redgearr'+strtrim(2*mm+1,2)+' = 0' )
  endelse
endfor

cmat = REDGE_CMAT
if not keyword_set(gmap) then cmat_orig = cmat

; -------------------- Filter New Edges -----------------------------;

filtersize= 2.5
for mm=0,nALL-1 do begin
  gg = execute( 'if keyword_set(redgearr'+strtrim(2*mm+1,2)+') then redgearr'+$
                strtrim(2*mm+1,2)+' = medianfilter(redgearr'+strtrim(2*mm+1,2)+$
                ',filtersize,rind)' )
endfor

;--------------------- Adjust New Edges -----------------------------;

if keyword_set(findfile('*_bsa')) then begin
  restore, filefits
  _dr = dr
  _dl = dl
  _dt1 = dt1
  _dt3 = dt3
  _dt5 = dt5
  _dt7 = dt7
  _dt9 = dt9
  ;cmat_orig = cmat
  if (_dr ne 0 or _dl ne 0) then goto, adjust else goto, graph

endif else begin
  dt1 = 0.
  dt3 = 0.
  dt5 = 0.
  dt7 = 0.
  dt9 = 0.
  dr = 0.
  dl = 0.
endelse

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
  gg = execute( 'if keyword_set(redgearr'+strtrim(2*mm+1,2)+$
                ') then yes=1 else yes=0' )
  if keyword_set(yes) then begin
    gg = execute( 'redge_recalc = linsam_to_redge_encke( cmat, redgearr'+$
                  strtrim(2*mm+1,2)+'[0:1,*], redgearr'+strtrim(2*mm+1,2)+$
                  '[2:3,*], redgearr'+strtrim(2*mm+1,2)+$
                  '[4:5,*], redge1_sigma=redge2_sigma, filestem=stemNEW )' )
    gg = execute( 'redgearr'+strtrim(2*mm+1,2)+'[10:11,*] = redge_recalc' )
    gg = execute( 'redgearr'+strtrim(2*mm+1,2)+'[12:13,*] = redge2_sigma' )
  endif
endfor

;if keyword_set(gmap) then goto, globalmap2

goto, nextplot

;----------------'nextplot' goto statement----------------------

nextplot:
if !d.name eq 'X' then window, xs=winx, ys=winy

   _dt1 = dt1
   _dt3 = dt3
   _dt5 = dt5
   _dt7 = dt7
   _dt9 = dt9

;   This handles the graph parameters 
!p.charsize=1.5
!p.multi = [0,1,nedges] 
!x.margin = [13,3]
!y.margin = [0,0]
!y.omargin = [4,2]

clr = [ ctred(), ctwhite(), ctgreen() ]


;-------------------Graphing loop-------------------------
graph:

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
  gg = execute( 'if keyword_set(redgearr'+strtrim(mm,2)+$
                ') then yes=1 else yes=0' )
  if keyword_set(yes) then begin
    gg = execute( 'lmin = min( [ lmin, min(redgearr'+strtrim(mm,2)+'[rind,*]) ] )' )
    gg = execute( 'lmax = max( [ lmax, max(redgearr'+strtrim(mm,2)+'[rind,*]) ] )' )
    ;; Note that we use rmin45 for mm=4 and for mm=5
    ALLind = strtrim(fix(mm/2)*2,2) + strtrim(fix(mm/2)*2+1,2)
    gg = execute( 'rmin'+ALLind+' = min( [rmin'+ALLind+', min(redgearr'+$
                  strtrim(mm,2)+'[rind+1,*]) ] )' )
    gg = execute( 'rmax'+ALLind+' = max( [rmax'+ALLind+', max(redgearr'+$
                  strtrim(mm,2)+'[rind+1,*]) ] )' )
  endif
endfor

xr = [lmin,lmax] + (lmax-lmin)*[-.1,.1]

for mm=nALL-1,0,-1 do begin
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
    gg = execute( 'yr = [rmin'+ALLind+',rmax'+ALLind+'] + (rmax'+ALLind+$
                  '-rmin'+ALLind+')*[-.1,.1]' )
 plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[mm], xtickn=xtn
    for nn=0,1 do gg = execute( 'if keyword_set(redgearr'+strtrim(2*mm+nn,2)+$
                                ') then oplot,redgearr'+strtrim(2*mm+nn,2)+$
                                '[rind,*]+dt'+strtrim(2*mm+1,2)+'*'+strtrim(nn,2)+',redgearr'+$
                                strtrim(2*mm+nn,2)+'[rind+1,*],color=clr['+$
                                strtrim(nn,2)+']' )
  endif
endfor

goto, mainmenu

;-----------------'mainmenu' goto statement----------------

mainmenu:
reply1 = ''
while reply1 eq '' do begin
  print, 'The following are values for dr, dl, dt1, dt3, dt5, dt7, and dt9:'
  print, dr, dl, dt1, dt3, dt5, dt7, dt9
  print, 'Adjust curve in [r]adius (vertical offset), [l]ongitude (twist), '
  print, 'various [t]imes (horizontal offsets), '
  print, '[a]dd another image, [c]reate PS image, [z] save global .dat file, '
  print, '[s]ave this pointing, return to [o]riginal pointing, or [q]uit?  '
  print, '(r/l/t/a/c/z/s/o/q)'
  read, reply1
  case reply1 of
      'q': goto, quit
      'o': begin
          ;cmat = cmat_orig
          ;dr = 0
          ;dl = 0
          ;dt = 0
          ;for mm=0,nALL-1 do
             ; new, changed redgearr now eq older redgearr
             ;gg = execute ( 'redgearr(mm*2+1) = redgearr(mm*2)')
         ;endfor
          print, 'This option has not been fully coded.'
          goto, mainmenu
      end
       's':begin
           sfile = imageNEW + '_bsa'
           save, dr, dl, dt1, dt3, dt5, dt7, dt9, filename=sfile
           print, 'Pointing offsets saved to '+imageNEW+'_bsa'
           ;reply5 = ''
           ;while reply5 eq '' do begin
           ;    print, 'Overwrite to creat a new .edge_linsam file? (y/n)'
           ;    read, reply5
           ;    case reply5 of
           ;        'y':overwrite=1
           ;        'n':overwrite=0
           ;        else: reply5 = ''
           ;    endcase
           ;endwhile
           ;if keyword_set(keeler) then begin
           ;    gap='K'
           ;    inout = ['I','O']
           ;endif else if keyword_set(encke) then begin
           ;    gap = 'E'
           ;    inout = ['I','O']
           ;endif else if keyword_set(aredge) then begin
           ;    gap = 'R'
           ;    inout = ['I','O']
           ;endif else stop, 'Please set either encke=1 or keeler=1 or aredge=1'
           ;for j=0,n_elements(inout)-1 do begin
           ;savefile = stemNEW + '.edge' + gap + inout[j]
           ;redge_cmat = cmat
           ;redge_sigma = redge2_sigma
           ;redge = redge_recalc
           ;save, redge, redge_sigma, redge_cmat, filename=savefile
       ;end
           ;print, '.edge_linsam file saved'
           ;goto, add
                                ;Want to reassign dr, dl, dt so that
                                ;when restarted, the program thinks
                                ;they're starting off as 0 when
                                ;they're at these settings
           goto, mainmenu
       end
       'z':begin
           print, 'This option has not been fully coded.'
           goto, mainmenu
       end
       'c': begin
           print, 'This option has not been fully coded.'
           goto, mainmenu
       end
       'a':begin


         for j=0,n_elements(titles)-1 do begin
          ALLnew[where(titles[j] eq ALLabbrev)] = 0
         endfor 

         foo = where( ALLcount ge 2, count )
         if count gt 0 then begin
           sfile = imageNEW+'_bsa'
           if keyword_set(findfile(sfile)) then begin
             if ( dr ne _dr or dl ne _dl or dt1 ne _dt1 or dt3 ne _dt3 or dt5 ne _dt5 or dt7 ne _dt7 or dt9 ne _dt9 ) then begin
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
             gg = execute( 'redgearr'+strtrim(2*mm+1,2)+'[rind,*] = redgearr'+$
                           strtrim(2*mm+1,2)+'[rind,*]+dt+strtrim(2*mm+1,2)' )
             gg = execute( 'redgearr'+strtrim(2*mm,2)+' = [[redgearr'+$
                           strtrim(2*mm,2)+'],[redgearr'+strtrim(2*mm+1,2)+$
                           ']]' )
             gg = execute( 'redgearr'+strtrim(2*mm,2)+' = redgearr'+$
                           strtrim(2*mm,2)+'[*,sort(redgearr'+strtrim(2*mm,2)+$
                           '[rind,*])]' )
           endif
           if keyword_set(overwrite) then save, dr, dl, dt1, dt3, dt5, dt7, dt9, filename=sfile
         endfor

         foo = where( ALLcount eq 1, count )
         if count gt 0 then for m=0,count-1 do begin
           mm = foo[m]
           if (where( titles eq ALLabbrev[mm] ))[0] ne -1 then begin
             gg = execute( 'redgearr'+strtrim(2*mm,2)+' = redgearr'+$
                           strtrim(2*mm+1,2) )
             gg = execute( 'redgearr'+strtrim(2*mm,2)+'[rind,*] = redgearr'+$
                          strtrim(2*mm,2)+'[rind,*]+dt+strtrim(2*mm+1,2)' )
           endif
           if keyword_set(overwrite) then save, dr, dl, dt1, dt3, dt5, dt7, dt9, filename=sfile
         endfor
         goto, add
     end
     't': begin
         type = [ 'time', 'degrees of longitude' ]
         reply2 = ''
         print, 'Enter '+type[0]+' offset in '+type[1]+' (or "q" to quit).'
         read, reply2
         if reply2 eq 'q' then goto, quit
         reply2 = float(reply2)
         reply4 = ''
         while reply4 eq '' do begin
             print, 'The following are values for dt1, dt3, dt5, dt7, and dt9:'
             print, dt1, dt3, dt5, dt7, dt9
            print, 'Adjust time for [1]Encke Inner, [3]Encke Outer, [5]Keeler Inner, '
            print, '[7]Keeler Outer, [9]Outer Edge of A Ring, or [q]uit.'
            print, '(1/3/5/7/9/q)'
            read, reply4
            case reply4 of
              '1': dt1 = dt1 + reply2
              '3': dt3 = dt3 + reply2
              '5': dt5 = dt5 + reply2
              '7': dt7 = dt7 + reply2
              '9': dt9 = dt9 + reply2
              'q': goto, quit
          endcase
      endwhile
      goto, graph
  end
      'l': begin
          type = [ 'longitudinal', 'pixels' ]
          reply2 = ''
          print, 'Enter '+type[0]+' offset in '+type[1]+' (or "q" to quit).'
          read, reply2
          if reply2 eq 'q' then goto, quit
          reply2 = float(reply2)
          _dl = reply2
          dl = dl + _dl
          goto, adjust
      end
      'r': begin
          type = [ 'radial', 'pixels' ]
          reply2 = ''
          print, 'Enter '+type[0]+' offset in '+type[1]+' (or "q" to quit).'
          read, reply2
          if reply2 eq 'q' then goto, quit
          reply2 = float(reply2)
          _dr = reply2
          dr = dr + _dr
          goto, adjust
      end
  endcase
endwhile

print, 'This program is working so far.'

quit:
wdelete = 0
!x.margin = [10,3]
!y.margin = [4,2]
!y.omargin = [0,0]

end
