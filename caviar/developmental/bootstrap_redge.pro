if not keyword_set(nedges) then nedges = 5
if not keyword_set(savefiles) then begin
  savefiles = [ 'N1532373063_1_cal.edge', 'N1532373096_1_cal.edge' ]
  filestems = strmid( savefiles, 0, 11 )
endif
nsf = n_elements(savefiles)

; Get parameters about file #1 that draw_arrows.pro will need
@get_sat_prepare
@get_cam_params
vobs_planet = [0.0d0,0.0d0,0.0d0]
restore, 'spreadsheet.sav'
ff = (where( strmid(data[0,*],0,11) eq strmid(savefiles[1],0,11), count ))[0]
if count ne 1 then stop
gg = (where( legend eq 'Instrument_Mode_ID', count ))[0]
if count ne 1 then stop
case data[gg,ff] of
  'FULL': nl = 1024
  'SUM2': nl = 512
  'SUM4': nl = 256
  else: stop
endcase
restore, 'stretch.sav'
ff = (where( strmid(filenames,0,11) eq strmid(savefiles[1],0,11), count ))[0]
if count ne 1 then stop
keywords = _keywords[ff]
restore, 'et.sav'
et = _et[ff]

; Define a structure array redgearr containing all 4 variables in each 
; .edge?_linsam file for each redge for each image, plus initial rad/lon.
for kkk=0,nedges-1 do begin
  for jjj=0,nsf-1 do begin
    restore, savefiles[jjj]+strtrim(kkk+1,2)+'_linsam'
    ; Calculate initial values in rad/lon, from initial cmat.
    redge1 = linsam_to_redge( redge_cmat, redge_linsam, redge_sigma_linsam, $
                              redge_dsigma_linsam, redge1_sigma=redge1_sigma, $
                              filestem=filestems[jjj])
    _redgearr = { redge_linsam: redge_linsam, $
                  redge_sigma_linsam:redge_sigma_linsam, $
                  redge_dsigma_linsam:redge_dsigma_linsam, $
                  redge_cmat:redge_cmat, $
                  redge1:redge1, $
                  redge1_sigma:redge1_sigma, $
                  redge2:redge1, $
                  redge2_sigma:redge1_sigma }
    if jjj eq 0 then begin
      __redgearr = _redgearr
    endif else begin
      __redgearr = [ __redgearr, _redgearr ]
    endelse
  endfor
  if kkk eq 0 then begin
    redgearr = __redgearr
  endif else begin
    redgearr = [ [redgearr], [__redgearr] ]
  endelse
endfor

; Check that, for each image, all redges use the same cmat
for jjj=0,nsf-1 do for p=0,2 do for q=0,2 do begin
  if stddev( (redgearr.redge_cmat)[p,q,jjj,*] ) ne 0 then stop, p, q, jjj
endfor
; Define current cmat and save original cmat (any cmat for jjj=1 will do)
cmat = (redgearr.redge_cmat)[*,*,1,0]
cmat_orig = cmat

if keyword_set(findfile(savefiles[1]+'_bs')) then begin
  restore, savefiles[1]+'_bs'
  _dr = dr
  _dl = dl
  goto, adjust
endif else begin
  dt = 0.
  dr = 0.
  dl = 0.
endelse

nextplot:
if keyword_set(plot_orig) then rind = 4 else rind = 6
;if !d.name eq 'X' then window, xs=1800, ys=1150
if !d.name eq 'X' then window, xs=1100, ys=600
; For each edge (kkk) plot all images (jjj) on the same graph
_dt = (redgearr.(rind))[0,*,*,*] * 0
_dt[*,*,1,*] = dt
lmin = min( (redgearr.(rind))[0,*,*,*] + _dt )
lmax = max( (redgearr.(rind))[0,*,*,*] + _dt )
xr = [lmin,lmax] + (lmax-lmin)*[-.1,.1]
!p.multi = [0,1,nedges]
!x.margin = [13,3]
!y.margin = [0,0]
!y.omargin = [4,2]
if nedges ge 3 then !p.charsize = 1.5
ytit = [ 'Gap Inner', 'Ringlet 1 Inner', 'Ringlet 1 Outer', $
         'Ringlet 2 Center', 'Gap Outer' ]
clr = [ ctred(), ctwhite(), ctgreen() ]
for kkk=nedges-1,0,-1 do begin
  if kkk eq 0 then begin
    xtn = '' 
    xtit = 'Co-Rotating Longitude (!Uo!N)'
  endif else begin
    xtn = replicate(' ',20)
    xtit = ''
  endelse
  yr = tkm([ min( (redgearr.(rind))[1,*,*,kkk] - $
                  (redgearr.(rind+1))[1,*,*,kkk] ), $
             max( (redgearr.(rind))[1,*,*,kkk] + $
                  (redgearr.(rind+1))[1,*,*,kkk] ) ])
  plot_nosci, xr, yr, /nodata, /xs, /ys, xtit=xtit, ytit=ytit[kkk], xtickn=xtn
  for jjj=0,nsf-1 do begin
    if jjj eq 1 then _dt = dt else _dt = 0
    polyfill, noclip=0, color=ctgray(), $
              [ reform( (redgearr.(rind))[0,*,jjj,kkk] + _dt ), $
                reverse(reform( (redgearr.(rind))[0,*,jjj,kkk] + _dt )) ], $
              tkm([ reform( (redgearr.(rind))[1,*,jjj,kkk] - $
                            (redgearr.(rind+1))[1,*,jjj,kkk] ), $
                    reverse(reform( (redgearr.(rind))[1,*,jjj,kkk] + $
                                    (redgearr.(rind+1))[1,*,jjj,kkk] )) ])
    oplot, (redgearr.(rind))[0,*,jjj,kkk] + _dt, $
           tkm( (redgearr.(rind))[1,*,jjj,kkk] ), color=clr[jjj]
  endfor
endfor

; Now adjust cmat for the middle image (jjj=1)
reply1 = ''
while reply1 eq '' do begin
  print, 'dr = '+strtrim(dr,2)+', dl = '+strtrim(dl,2)+', dt = '+strtrim(dt,2)
  print, 'Adjust curve in [r]adius (vertical offset), [l]ongitude (twist), '+$
         '[t]ime (horizontal offset), '
  print, '[s]ave this pointing, or return to [o]riginal pointing?  '+$
         '(r/l/t/s/o/q)'
  read, reply1
  case reply1 of
    'r': type = [ 'radial', 'pixels' ]
    'l': type = [ 'longitudinal', 'pixels' ]
    't': type = [ 'time', 'degrees of longitude' ]
    's': begin
      save, dr, dl, dt, filename=savefiles[1]+'_bs'
      print, 'Pointing offsets saved to '+savefiles[1]+'_bs'
      goto, quit
    end
    'o': begin
      cmat = cmat_orig
      dr = 0.
      dl = 0.
      dt = 0.
      redgearr.redge2 = redgearr.redge1
      redgearr.redge2_sigma = redgearr.redge1_sigma
      goto, nextplot
    end
    'q': goto, quit
    else: reply1 = ''
  endcase
endwhile
reply2 = 0.
while reply2 eq 0 do begin
  print, 'Enter '+type[0]+' offset in '+type[1]+' (or "q" to quit).'
  read, reply2
  if reply2 eq 'q' then goto, quit
endwhile
_dr = 0.
_dl = 0.
case reply1 of
  'r': _dr = reply2
  'l': _dl = reply2
  't': dt = dt + reply2
endcase
dr = dr + _dr
dl = dl + _dl

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

;(redgearr.redge_cmat)[*,*,1,*] = rebin( cmat, 3, 3, 1, nedges )
for kkk=0,nedges-1 do begin
  redge2 = linsam_to_redge( cmat, (redgearr.redge_linsam)[*,*,1,kkk], $
                            (redgearr.redge_sigma_linsam)[*,*,1,kkk], $
                            (redgearr.redge_dsigma_linsam)[*,*,1,kkk], $
                            redge1_sigma=redge2_sigma, filestem=filestems[1])
  redgearr[1,kkk].redge2 = redge2
  redgearr[1,kkk].redge2_sigma = redge2_sigma
endfor
goto, nextplot

quit:
!x.margin = [10,3]
!y.margin = [4,2]
!y.omargin = [0,0]

end
