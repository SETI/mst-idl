; Use this to generate wavelet plots for directories that have in place a
; ring_rads_index.sav file.  Currently approved for:
; 046/RDHRESSCN
; 071/RDHRSSCHP
restore, 'stretch.sav'
image_name=filenames[jjj]
psname = strmid(image_name,0,11) + '_wavelet'
if not keyword_set(psnameextra) then psnameextra = ''
psname = psname + psnameextra
noplot = 1
@caviar  ;Loads ring_rads_index.sav, adjusts radi by radscan_radial_offset[jjj]
noplot = 0
if keyword_set(nokeep) then ring_rads_keep[*] = 0
.run restore_radscan
.run plot_radscan
.run preparewaveletplots  ; Sets up a PS plot when dolzr = 1
wavelet_niceplot=1
hang=1
wavelet_tit = strmid(image_name,0,11)
if not keyword_set(titextra) then titextra = ''
wavelet_tit = wavelet_tit + titextra
if keyword_set(notit) then wavelet_tit = ''
@run_wavelet1
ridges = wavelet_ridges( wave, radi, wavenum )
if not keyword_exists(showridges) and not keyword_set(dolzr) then showridges=1
if not keyword_set(showridges) then showridges=0
if keyword_set(dolzr) and showridges lt 2 then showridges=0
if keyword_set(showridges) then oplot, tkm(ridges[*,2]), ridges[*,3], ps=3
if keyword_set(ring_rads_sigma_manual) then begin &$
  foo = where( ring_rads_sigma_manual eq 0, count ) &$
endif else begin &$
  count = n_elements(ring_rads) &$
  foo = lindgen(count); &$
;endelse
if keyword_set(radscan_sigma) then if count ge 1 then $
   plot_wavemodel, ring_rads[foo], ring_rads_legend[foo], $
                   radscan_sigma[jjj], fit=fit, /mooncolor, $
                   ring_rads_index=ring_rads_index[foo], $
                   ring_rads_maxlam=ring_rads_maxlam[foo], $
                   shownew=shownew, showall=showall
if keyword_set(samplesdw) then begin &$
  if samplesdw eq 2 then begin &$
    xr1a = 25. &$
    xr1b = 150. &$
    xr1c = 110 &$
    xr1 = tkm(ring_rads[foo[0]]+xr1a) + findgen(xr1b-xr1a+1)/1000 &$
    xrq = poly( xr1*1e3, fit ) + $
                     ( (xr1-mean(xr1))^2 - (xr1[0]-mean(xr1))^2 )*xr1c &$
    oplot, xr1, xrq, co=get_mooncolor(ring_rads_legend[foo]), l=1, th=6 &$
  endif &$
  if keyword_set(samplesdw2) then begin &$
    xr1d = (where( abs(xr1-126.0) eq min(abs(xr1-126.0)) ))[0] &$
    oplot, [ tkm(ring_rads[foo[0]]), xr1[xr1d] ], [ 0, xrq[xr1d] ], l=2, th=5 &$
  endif &$
endif
foo = where( ring_rads_sigma_manual ne 0, count )
if count ge 1 then for j=0l,count-1 do plot_wavemodel, ring_rads[foo[j]], $
            ring_rads_legend[foo[j]], $
            ring_rads_sigma_manual[foo[j]], /mooncolor, $
            ring_rads_index=ring_rads_index[foo[j]], $
            ring_rads_maxlam=ring_rads_maxlam[foo[j]], $
            shownew=shownew, showall=showall
if keyword_set(_ring_rads_unexplained) then begin &$
  for j=0l,n_elements(_ring_rads_unexplained[0,0,*])-1 do begin &$
    oplot, _ring_rads_unexplained[0,*,j], _ring_rads_unexplained[1,*,j], $
           l=2, thick=3.5*!p.thick ;&$
  ;endfor &$
;endif
if keyword_set(radscan_nwakes) then if radscan_nwakes[jjj] gt 0 then begin &$
  sat = 618 &$
  @get_sat_coords &$
  plot_wakemodel, radi, keywords.ringplane_aimpoint_longitude, sat_polar[1], $
                  133586.0d0, nwakes=radscan_nwakes[jjj]; &$
;endif
if keyword_set(dolzr) then clzr

;end
