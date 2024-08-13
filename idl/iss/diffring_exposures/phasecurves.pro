; To show derivation of the background model curves:
; bg = 1
; allfilters = 1
; Set bgonly = 1 to show backgrounds but not ring exposures
; Set niceplot = 1 to make a nice plot

; To show derivation of the product between I/F and exposure times:
; ioverf = 1

; To show derivation of the phase function curves:
; allfilters = 1

if not keyword_set(winnum) then winnum = 0
if keyword_set(allfilters) then !p.multi = [0,4,3] else !p.multi = [0,2,2]
if !d.name eq 'X' then window, winnum, xs=1280, ys=1024
if !d.name eq 'X' then !p.charsize = 2
if keyword_set(niceplot) then begin
  xom = !x.omargin & yom = !y.omargin
  xm = !x.margin & ym = !y.margin
  !x.omargin = [10,3]
  !y.omargin = [4,2]
  !x.margin = 0
  !y.margin = 0
endif

; I/F = tau * albedo * phase / 4 / sin(beta)
; Throop and Esposito (1998).  beta is the ring opening angle

; Trace the approximate phase curve, averaging curves 1 and 2 
; from Cuzzi et al 1984 Fig. A2 (p.181)
alpha = findgen(10)*20
pp_ee = [ .4, .25, .15, .12, .12, .2, .5, 1.5, 7, 40 ]
pp_gg = [ .4, .25, .15, .12, .1, .12, .2, .5, 2.3, 13.3 ]
save, alpha, pp_ee, pp_gg, filename='~/idl/iss/diffring_exposures/phasecurves.sav'

; Lower limit exposure for saturation of background, for various filters as
; a function of phase angle.
bgpredict = ptrarr( 20, /allocate )
*(bgpredict[0]) = [ [1e-10,1e-10], [1e-10,1e-10] ]    ; E All Filters
*(bgpredict[1]) = [ [1e-10,1e-10], [1e-10,1e-10] ]    ; G All Filters
;*(bgpredict[2]) = [ [55,11], [110,10], [144,.5] ]    ; E CL1/CL2
*(bgpredict[2]) = [ [55,11], [110,10], [137,3], [146,.25] ]    ; E CL1/CL2
;*(bgpredict[3]) = [ [55,270], [130,60], [162,4.5] ]    ; G CL1/CL2
*(bgpredict[3]) = [ [55,206], [136,109], [162,4.5] ]    ; G CL1/CL2
;*(bgpredict[4]) = [ [55,18], [118,15], [143,1.5] ]  ; E RED
*(bgpredict[4]) = [ [55,18], [133,13.5], [145,.5] ]  ; E RED
*(bgpredict[5]) = [ [55,550], [122,260], [155,58] ]               ; G RED
;*(bgpredict[6]) = [ [55,220], [105,280], [144,22] ]    ; E BL1
*(bgpredict[6]) = [ [55,220], [105,280], [129,152], [146,8] ]    ; E BL1
*(bgpredict[7]) = [ [55,1100], [122,690], [154,262] ]              ; G BL1
;*(bgpredict[8]) = [ [55,34], [104,60], [144,3.3] ]    ; E GRN
*(bgpredict[8]) = [ [55,34], [104,60], [130,25], [146,1.2] ]    ; E GRN
*(bgpredict[9]) = [ [51,650], [123,380], [152,76] ]               ; G GRN
;*(bgpredict[10]) = [ [62,760], [123,600], [146,160] ]    ; E VIO
*(bgpredict[10]) = [ [62,760], [135,552], [145,75] ]    ; E VIO
;*(bgpredict[11]) = [ [51,30], [110,14], [145,1.3] ]   ; E IRP0
*(bgpredict[11]) = [ [51,30], [110,14], [136,5], [146,0.35] ]   ; E IRP0
;*(bgpredict[12]) = [ [51,30], [110,14], [145,1.3] ]   ; E IRP90
*(bgpredict[12]) = [ [51,30], [110,14], [136,5], [146,0.35] ]   ; E IRP90
;*(bgpredict[13]) = [ [62,20], [108,20], [144,1] ]      ; E IR1
*(bgpredict[13]) = [ [62,20], [108,20], [137,5.3], [145,0.35] ]      ; E IR1
*(bgpredict[14]) = [ [57,590], [125,320], [155,63] ]              ; G IR1
;*(bgpredict[15]) = [ [54,200], [145,2.5] ]             ; E IR2
*(bgpredict[15]) = [ [57.5,60.5], [133,10], [146,0.85] ]             ; E IR2
*(bgpredict[16]) = [ [55,920], [126,550], [155,159] ]              ; G IR2
;*(bgpredict[17]) = [ [100,120], [145,2.5] ]            ; E IR3
*(bgpredict[17]) = [ [100,29], [133,16.5], [145,2.5] ]            ; E IR3
*(bgpredict[18]) = [ [55,920], [126,550], [155,300] ]              ; G IR3
*(bgpredict[19]) = [ [52,590], [130,300] ]              ; G P0/P60/P120
; Nothing for All Filters
bgpredict = bgpredict[2:19]
save, bgpredict, filename='~/idl/iss/diffring_exposures/bgpredict.sav'

if keyword_set(ioverf) then begin
  ; ioverffac is the product between the measured I/F of the ring (scaled 
  ; to gain=12 and sum=2) and the actual exposure (scaled to saturation).
  ; It turns out to be a relatively constant number for each filter, 
  ; usually to within less than 10%.  
  ; For a given filter (name stored in filtertit), divide ioverffac by the 
  ; I/F (normalized to gain=12 and sum=2) to get the saturation exposure time 
  ; in seconds (or vice versa).  
  donormexpos = 1
  allfilters = 1
  filtertit = ''
  ioverffac = 0.
  ioverferr = 0.
  showsum = 1
  showgain = 1
endif

if keyword_set(allfilters) then filter = 0 else filter = 1
nextfilter:
if not keyword_exists(filter) then filter = 1
fooe = -1
foog = -1
run_diffring_exposures, /noplot, e_phase=e_phase, g_phase=g_phase, $
  fooe=fooe, foog=foog, ee=ee, gg=gg, norme=norme, normg=normg, $
  e_data=e_data, g_data=g_data, fname=filtername, filter=filter, $
  e_mnem=e_mnem, g_mnem=g_mnem, e_emission=e_emission, g_emission=g_emission
run_diffring_exposures, /noplot, /dn, ee=ee_dn, gg=gg_dn
run_diffring_exposures, /noplot, /bg, ee=ee_bg, gg=gg_bg
run_diffring_exposures, /noplot, /dn, /bg, ee=ee_dn_bg, gg=gg_dn_bg
norm = 3e-6
;print,filtername[filter]
if fooe[0] ne -1 then begin
  e_phasefunction = ee[fooe]*norme/norm
  e_normexpos = replicate( 1, n_elements(fooe) )
  ;foo = where( e_data[1,fooe] eq 'ISSWA', count )
  ;if count ne 0 then print,'E Ring WAC: '+strtrim(count,2)
  ;foo = where( e_data[1,fooe] eq 'ISSNA', count )
  ;if count ne 0 then print,'E Ring NAC: '+strtrim(count,2)
endif
if foog[0] ne -1 then begin
  g_phasefunction = gg[foog]*normg/norm
  g_normexpos = replicate( 1, n_elements(foog) )
  ;foo = where( g_data[1,foog] eq 'ISSWA', count )
  ;if count ne 0 then print,'G Ring WAC: '+strtrim(count,2)
  ;foo = where( g_data[1,foog] eq 'ISSNA', count )
  ;if count ne 0 then print,'G Ring NAC: '+strtrim(count,2)
endif

if keyword_set(donormexpos) then begin

  frac = .1;.25
  if keyword_set(allfilters) then begin
    ftit = ' - '+filtername[filter]
    if filter eq 0 then begin
      !p.multi = [0,5,4]
      !y.margin = [6,2]
    endif 
  endif else begin
    ftit = ''
  endelse
  solid_diamonds
  if keyword_set(ioverf) and filter eq 0 then begin
    filter = 1
    goto, nextfilter
  endif

  if fooe[0] ne -1 then begin
    ftit1 = ' ('+[ '', '0.3', '1.5', '10', '1.5', '10/40', '', '1.5', '1.5', $
                  '1', '1/4', '1/4', '', '', '', '' ]+')'
    gain = reform(strmid( e_data[6,*], 0, 3 ))
    sum = reform(strmid( e_data[7,*], 3, 1 ))
    foo = where( sum eq 'L', count )
    if count gt 0 then sum[foo] = '1'
    if keyword_set(allfilters) then begin
      xtit = 'Actual Exposure,!Cscaled to Saturation (sec)'
    endif else begin
      xtit = 'Actual Exposure, scaled to Saturation (sec)'
    endelse
    x = e_data[3,fooe]/1e3/ee_dn[fooe]*4096; * frac
    ;y = e_normexpos / (e_phasefunction*min(norme)/norme / $
    ;                   (gain[fooe]/12) * sum[fooe]^2/4)
    ytit = 'Model Normalized Exposure'
    y = x / ( e_normexpos / (e_phasefunction*sin(!pi/180)/norme / $
                             (gain[fooe]/12) * sum[fooe]^2/4) )
    if keyword_set(ioverf) then begin
      !x.margin = [13,3]
      ftit1[*] = ''
      ytit = 'I/F (for gain=12 and sum=2)!CProduct with x-axis'
      y = ee[fooe] / (gain[fooe]/12) * sum[fooe]^2/4 * x
    endif
    if keyword_set(reconstruct) then begin
      xtit = 'Actual Exposure (sec)'
      x = e_data[3,fooe]/1e3
      ytit = 'Reconstructed Normalized Exposure'
      y = interpol( pp_ee, alpha, e_phase[fooe] ) * $
          sin(!pi/180)/norme / (gain[fooe]/12) * sum[fooe]^2/4 * $
          e_data[3,fooe]/1e3
    endif
    if keyword_set(fracexpos) then begin
      xtit = 'Exposure Fraction'
      x = ee_dn[fooe] / 4096
    endif
    if keyword_set(bg) then begin
      ytit = 'Phase Angle'
      y = e_phase[fooe]
      xtit = 'Actual Exposure, scaled to Saturation (sec)'
      x = e_data[3,fooe]/1e3/ee_dn_bg[fooe]*4096
      ;x = ee_bg[fooe]
    endif
    mn = mean( y )
    sd = stddev( y )
    if keyword_set(ioverf) then begin
      filtertit = [ filtertit, 'E Ring'+ftit ]
      ioverffac = [ ioverffac, mn ]
      ioverferr = [ ioverferr, sd ]
    endif
    plot, /xlog, x, y, /nodata, /xs, /ys, $
          xr=10^((max(alog10(x))-min(alog10(x)))*[-.1,.1]+$
                 [min(alog10(x)),max(alog10(x))]), $
          yr=(max(y)-min(y))*[-.1,.1]+[min(y),max(y)], $
          tit='E Ring'+ftit+ftit1[filter], xtit=xtit, ytit=ytit
    dx = floor(!x.crange[1]) - ceil(!x.crange[0])
    if dx eq 0 then begin
      axis_halflog, xaxis=0 
    endif else if dx eq -1 then begin
      axis_halflog, xaxis=0, /halflog25
    endif
    oplot, x, y, ps=8
    oplot, 10^!x.crange, mean(y)*[1,1]
    oplot, 10^!x.crange, mn*[1,1]
    if keyword_set(ioverf) then begin
      text = string( mn, fo='(F8.6)' ) + pmsym() + string( sd, fo='(F8.6)' )
    endif else begin
      text = string( sd/mn*100, fo='(F5.1)' ) + '% variation'
    endelse
    xyouts, gmean(10^!x.crange), max(y), /data, chars=.5, align=.5, text
    if keyword_set(showgain) then begin
      pp = where( e_data[6,fooe] eq '29 ELECTRONS PER DN', count )
      if count gt 0 then oplot, x[pp], y[pp], ps=4,  co=ctred()
    endif
    if keyword_set(showsum) then begin
      qq = where( e_data[7,fooe] eq 'FULL', count )
      if count gt 0 then oplot, x[qq], y[qq], ps=6,  co=ctgreen()
    endif
  endif
;stop
  if foog[0] ne -1 then begin
    ftit1 = ' ('+[ '', '1.14/2', '100/10', '100/15', '100/10', '', '19', '', $
                   '', '100/10', '100/19', '19', '13.8/5', '13.8/5', '13.8/5', $
                   '13.8/5' ]+')'
    gain = reform(strmid( g_data[6,*], 0, 3 ))
    sum = reform(strmid( g_data[7,*], 3, 1 ))
    foo = where( sum eq 'L', count )
    if count gt 0 then sum[foo] = '1'
    if keyword_set(allfilters) then begin
      xtit = 'Actual Exposure,!Cscaled to Saturation (sec)'
    endif else begin
      xtit = 'Actual Exposure, scaled to Saturation (sec)'
    endelse
    x = g_data[3,foog]/1e3/gg_dn[foog]*4096; * frac
    ;y = g_normexpos / (g_phasefunction*min(normg)/normg / $
    ;                   (gain[foog]/12) * sum[foog]^2/4)
    ytit = 'Model Normalized Exposure'
    y = x / ( g_normexpos / (g_phasefunction*sin(!pi/180)/normg / $
                             (gain[foog]/12) * sum[foog]^2/4) )
    if keyword_set(ioverf) then begin
      !x.margin = [13,3]
      ftit1[*] = ''
      ytit = 'I/F (for gain=12 and sum=2)!CProduct with x-axis'
      y = gg[foog] / (gain[foog]/12) * sum[foog]^2/4 * x
    endif
    if keyword_set(reconstruct) then begin
      xtit = 'Actual Exposure (sec)'
      x = g_data[3,foog]/1e3
      ytit = 'Reconstructed Normalized Exposure'
      y = interpol( pp_ee, alpha, g_phase[foog] ) * $
          sin(!pi/180)/normg / (gain[foog]/12) * sum[foog]^2/4 * $
          g_data[3,foog]/1e3
    endif
    if keyword_set(fracexpos) then begin
      xtit = 'Exposure Fraction'
      x = gg_dn[foog] / 4096
    endif
    if keyword_set(bg) then begin
      ytit = 'Phase Angle'
      y = g_phase[foog]
      xtit = 'Actual Exposure, scaled to Saturation (sec)'
      x = g_data[3,foog]/1e3/gg_dn_bg[foog]*4096
      ;x = gg_bg[foog]
    endif
    a=where( strmid(g_data[11,foog],10,9) eq 'RETHIEQPL', counta )
    nota=where( strmid(g_data[11,foog],10,9) ne 'RETHIEQPL' )
    b=where( g_data[8,foog] eq 'TABLE', countb )
    mn = mean( y[nota] )
    sd = stddev( y[nota] )
    if keyword_set(ioverf) then begin
      filtertit = [ filtertit, 'G Ring'+ftit ]
      ioverffac = [ ioverffac, mn ]
      ioverferr = [ ioverferr, sd ]
    endif
    plot, /xlog, x, y, /nodata, /xs, /ys, $
          xr=10^((max(alog10(x))-min(alog10(x)))*[-.1,.1]+$
                 [min(alog10(x)),max(alog10(x))]), $
          yr=(max(y)-min(y))*[-.1,.1]+[min(y),max(y)], $
          tit='G Ring'+ftit+ftit1[filter], xtit=xtit, ytit=ytit
    dx = floor(!x.crange[1]) - ceil(!x.crange[0])
    if dx eq 0 then begin
      axis_halflog, xaxis=0 
    endif else if dx eq -1 then begin
      axis_halflog, xaxis=0, /halflog25
    endif
    oplot, x[nota], y[nota], ps=8
    if counta gt 0 then oplot, x[a], y[a], ps=4
    if countb gt 0 then oplot, x[b], y[b], ps=3
    oplot, 10^!x.crange, mn*[1,1]
    if keyword_set(ioverf) then begin
      text = string( mn, fo='(F8.6)' ) + pmsym() + string( sd, fo='(F8.6)' )
    endif else begin
      text = string( sd/mn*100, fo='(F5.1)' ) + '% variation'
    endelse
    xyouts, gmean(10^!x.crange), max(y), /data, chars=.5, align=.5, text
    if keyword_set(showgain) then begin
      pp = where( g_data[6,foog] eq '29 ELECTRONS PER DN', count )
      if count gt 0 then oplot, x[pp], y[pp], ps=4,  co=ctred()
    endif
    if keyword_set(showsum) then begin
      qq = where( g_data[7,foog] eq 'FULL', count )
      if count gt 0 then oplot, x[qq], y[qq], ps=6,  co=ctgreen()
    endif
  endif
;stop
  if keyword_set(allfilters) then begin
    if filter eq 5 then filter = 6
    if filter eq 11 then filter = 14
    if filter eq 15 then goto, finish else begin
      filter = filter + 1
      goto, nextfilter
    endelse
  endif else goto, finish

  if 4 eq 5 then begin
    finish:
    if keyword_set(ioverf) then begin
      filtertit = clip(filtertit)
      ioverffac = clip(ioverffac)
      ioverferr = clip(ioverferr)
      save, filtertit, ioverffac, ioverferr, $
        filename='~/idl/iss/diffring_exposures/ioverffac.sav'
      plot_blank
      oplot, /noclip, [0], [.8], ps=8
      oplot, /noclip, [0], [.65], ps=4
      oplot, /noclip, [0], [.5], ps=3
      oplot, /noclip, [0], [.35], ps=4, co=ctred()
      oplot, /noclip, [0], [.2], ps=6, co=ctgreen()
      chsz = 3. * !d.y_ch_size / (!d.y_size-6)
      xyouts, chars=1, [.1], [.8]-chsz*.5, 'Most observations'
      xyouts, chars=1, [.1], [.65]-chsz*.5, 'RETHIEQPL (Revs 19 and 20)'
      xyouts, chars=1, [.1], [.5]-chsz*.5, 'TABLE data conversion (others are 12BIT)'
      xyouts, chars=1, [.1], [.35]-chsz*.5, 'Gain=29 e/DN (others are 12)'
      xyouts, chars=1, [.1], [.2]-chsz*.5, 'FULL resolution (others are SUM2)'
    endif
    retall
  endif

endif

if keyword_set(bg) then begin
  ytit = 'Saturation Exposure (sec)'
  if not keyword_set(bgonly) then ytit = 'Normalized '+ytit
  if keyword_set(bgioverf) then ytit = 'Background I/F'
  if keyword_set(actual) then ytit = 'Actual Exposure, scaled to Saturation (sec)'
  restore, '~/idl/iss/diffring_exposures/ioverffac.sav'
  yy = 0
  if keyword_set(niceplot) and filter eq 0 then begin
    filter = 1
    goto, nextfilter
  endif
  if fooe[0] ne -1 then begin
    jj = (where( filtertit eq 'E Ring - '+filtername[filter] ))[0]
    ye = ioverffac[jj] / ee[fooe] / norme * sin(!pi/180)
    if keyword_set(bgioverf) then ye = ee[fooe]
    if keyword_set(actual) then ye = reform(e_data[3,fooe]/1e3/ee_dn[fooe]*4096)
    ye_bg = ioverffac[jj] / ee_bg[fooe]
    if keyword_set(bgioverf) then ye_bg = ee_bg[fooe]
    if keyword_set(actual) then ye_bg = reform(e_data[3,fooe]/1e3/ee_dn_bg[fooe]*4096)
    yy = [ yy, ye, ye_bg ]
  endif else ye = 1e-10
  if foog[0] ne -1 then begin
    jj = (where( filtertit eq 'G Ring - '+filtername[filter] ))[0]
    yg = ioverffac[jj] / gg[foog] / normg * sin(!pi/180)
    if keyword_set(bgioverf) then yg = gg[foog]
    if keyword_set(actual) then yg = reform(g_data[3,foog]/1e3/gg_dn[foog]*4096)
    yg_bg = ioverffac[jj] / gg_bg[foog]
    if keyword_set(bgioverf) then yg_bg = gg_bg[foog]
    if keyword_set(actual) then yg_bg = reform(g_data[3,foog]/1e3/gg_dn_bg[foog]*4096)
    yy = [ yy, yg, yg_bg ]
  endif else yg = 1e-10
  if n_elements(yy) gt 1 then yy = clip(yy)
;  if keyword_set(niceplot) then yr = [ 20000, .4 ] else begin
  if keyword_set(niceplot) then yr = [ 2000, .2 ] else begin
    yr = 10^( ( max(alog10([yy])) - min(alog10([yy])) )*[-.1,.1] + $
              [ min(alog10([yy])), max(alog10([yy])) ] )
    if not keyword_set(bgioverf) then yr = reverse(yr)
  endelse
endif else begin
  ytit = 'Phase Function'
  if fooe[0] ne -1 then begin
    ye = e_phasefunction
  endif else ye = 1e-10
  if foog[0] ne -1 then begin
    yg = g_phasefunction
  endif else yg = 1e-10
  yr = [.05,40]
endelse
if fooe[0] ne -1 or foog[0] ne -1 then begin
  if keyword_set(niceplot) then begin
    tit = ''
    yma = 0
    if (where(!p.multi[0] eq [1,2,3,4,5]))[0] ne -1 then begin
      xtit = 'Phase Angle'
      xtn = [ ' ', '', '', '', '', '', ' ' ]
    endif else begin
      xtit = ''
      xtn = replicate(' ',20)
    endelse
    if (where(!p.multi[0] eq [0,4,8]))[0] ne -1 then begin
      _ytit = ytit
      ytn = ''
    endif else begin
      _ytit = ''
      ytn = replicate(' ',20)
    endelse
  endif else begin
    ;tit = '!MtA/4 = 3 x 10!U-6!N!C(90-e!DE!N)!Dmin!N = 1!Uo!N'
    tit = filtername[filter]
    xtit = 'Phase Angle'
    _ytit = ytit
    yma = [4,4]
  endelse
  plot, alpha, pp_ee, /nodata, /ylog, /xs, /ys, xticki=30, yr=yr, $
        xtit=xtit, ytit=_ytit, yma=yma, tit=tit, $
        xtickn=xtn, ytickn=ytn
  if not keyword_set(bg) then oplot, alpha, pp_ee
  if not keyword_set(bg) then oplot, alpha, pp_gg
endif

solid_diamonds
if fooe[0] ne -1 then begin
  if not keyword_set(bgonly) then begin
    oplot, e_phase[fooe], ye, ps=8, co=ctcyan()
  endif
  if keyword_set(bg) then begin
    oplot, e_phase[fooe], ye_bg, ps=4, co=ctcyan()
    jj = (where( filtertit eq 'E Ring - '+filtername[filter] ))[0]
    oplot, (*(bgpredict[jj]))[0,*], (*(bgpredict[jj]))[1,*], co=ctcyan(), l=2
    if not keyword_set(bgonly) then begin
      oplot, alpha, ioverffac[jj] / pp_ee / norm * sin(!pi/180), co=ctcyan()
    endif
    if keyword_set(niceplot) then xyouts, 12, $
           !y.crange[1]+(!y.crange[0]-!y.crange[1])/3, filtername[filter]
  endif
endif
if foog[0] ne -1 then begin
  if not keyword_set(bgonly) then begin
    oplot, g_phase[foog], yg, ps=8, co=ctred()
  endif
  if keyword_set(bg) then begin
    oplot, g_phase[foog], yg_bg, ps=4, co=ctred()
    jj = (where( filtertit eq 'G Ring - '+filtername[filter] ))[0]
    oplot, (*(bgpredict[jj]))[0,*], (*(bgpredict[jj]))[1,*], co=ctred(), l=2
    if not keyword_set(bgonly) then begin
      oplot, alpha, ioverffac[jj] / pp_gg / norm *  sin(!pi/180), co=ctred()
    endif
    if keyword_set(niceplot) then xyouts, 12, $
           !y.crange[1]+(!y.crange[0]-!y.crange[1])/3, filtername[filter]
  endif
endif

if keyword_set(showpredictions) then begin
  alpha1 = [ 153, 159, 152, 148, 133, 134, 159, 152, 148, 132, 135, 163, 163, $
             163, 104, 107, 110, 110, 145, 80, 142, 78, 113, 115, 125, 118, $
             60, 60, 115, 85.1, 80.9, 28.4, 46.5, 84.4, 103.7 ]
  pp1 = [ 2, 7, 2, 2, 1, 1, 7, 2, 2, 1, 1, 15, 15, 15, 0.25, 0.3, 0.35, 0.35, $
          2, 0.15, 2, 0.15, 0.5, 0.5, 0.5, 0.5, 0.1, 0.1, 0.5, 0.15, 0.15, $
          0.2, 0.15, 0.15, 0.25 ]
  oplot, alpha1, pp1, ps=4
endif

if keyword_set(altnorm) then begin

  norm = 1e-6
  norme[*] = sin(!pi/180/3)
  if fooe[0] ne -1 or foog[0] ne -1 then begin
    ;tit = '!MtA/4 = 1 x 10!U-6!N!C(90-e!DE!N)!Dmin!N = 0.333!Uo!N'
    tit = filtername[filter]
    plot, alpha, pp_ee, /ylog, /xs, /ys, xticki=30, yr=[.05,40], $
          xtit='Phase Angle', ytickn=replicate(' ',20), yma=[4,4], tit=tit
  endif

  if fooe[0] ne -1 then begin
    oplot, e_phase[fooe], ee[fooe]*norme/norm, ps=4, co=ctcyan()
  endif
  if foog[0] ne -1 then begin
    oplot, g_phase[foog], gg[foog]*normg/norm, ps=4, co=ctred()
  endif

endif
;stop
if keyword_set(allfilters) then begin
  if filter eq 5 then filter = 6
  if filter eq 11 then filter = 14
  if filter eq 15 then goto, finish1 else begin
    filter = filter + 1
    goto, nextfilter
  endelse
endif else goto, finish1

finish1:
if keyword_set(niceplot) then begin
  plot_blank
  oplot, [.2], [.7], ps=8, co=ctcyan()
  oplot, [.2], [.6], ps=4, co=ctcyan()
  oplot, [.2], [.325], ps=8, co=ctred()
  oplot, [.2], [.225], ps=4, co=ctred()
  chsz = 3. * !d.y_ch_size / (!d.y_size-6)
  xyouts, [.3], [.7]-chsz/2, co=ctcyan(), 'E Ring (WAC)', chars=1.5
  xyouts, [.3], [.6]-chsz/2, co=ctcyan(), 'E Ring (WAC)!CBackground', chars=1.5
  xyouts, [.3], [.325]-chsz/2, co=ctred(), 'G Ring (NAC)', chars=1.5
  xyouts, [.3], [.225]-chsz/2, co=ctred(), 'G Ring (NAC)!CBackground', chars=1.5
  !x.omargin = xom & !y.omargin = yom
  !x.margin = xm & !y.margin = ym
endif

end
