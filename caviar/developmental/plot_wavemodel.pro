pro plot_wavemodel, ring_rads, ring_rads_legend, sigma, whichres=_whichres, $
                    notres=notres, fit=fit, mooncolor=mooncolor, color=color, $
                    xr=_xr, debug=debug, tkmradi=tkmradi, $
                    ring_rads_index=ring_rads_index, shownew=shownew, $
                    ring_rads_maxlam=ring_rads_maxlam, showall=showall

if n_params() eq 0 then begin
  print, 'Syntax:  PLOT_WAVEMODEL, ring_rads, ring_rads_legend, sigma'
  retall
endif

nrr = n_elements(ring_rads)
if keyword_set(_whichres) then whichres=_whichres else whichres=indgen(nrr)
bold = intarr(nrr)
if whichres[0] eq -1 then return
if keyword_set(ring_rads_index) then begin
  if keyword_set(shownew) then begin
    ; 0 = Wave does not fall within an image (or not yet categorized)
    foo = where( ring_rads_index ne 0, count )
    if count ge 0 then whichres = vec_remove( whichres, foo ) else stop
  endif else if not keyword_set(showall) then begin
    ; 4 = Resonance associated with a gap and/or ringlet
    ; 5 = Resonance does not fall within a wave-propagating region
    foo = where( ring_rads_index eq 4 or ring_rads_index eq 5, count )
    if count eq n_elements(whichres) then begin
      stop, 'Removing all elements from whichres?'
    endif else if count ge 0 then whichres = vec_remove( whichres, foo )
    ; 1 = Wave observed
    ; 2 = Wave not observed, possibly due to other structure
    ; 3 = Wave should be observed and is not
    bold = ring_rads_index
    foo = where( bold eq 2 or bold eq 3, count )
    if count gt 0 then bold[foo] = 0
    foo = where( bold eq 1, count )
    if count gt 0 then bold[foo] = 1
  endif
endif else if keyword_exists(notres) then if notres[0] ne -1 then begin
  whichres = vec_remove( whichres, notres )
endif
nwr = n_elements(whichres)
fit = dblarr( 2, nwr )

if not keyword_exists(mooncolor) then mooncolor=0
saturn_constants, gm=gm, prad=rsat, j2=j2
cap_g = 6.674d-8  ; cm^3 / g / s^2
if !p.thick eq 0 then !p.thick = 1
for j=0,nwr-1 do begin
  k = whichres[j]
  if bold[k] eq 1 then thick=3.5*!p.thick else thick=!p.thick
  colon = strpos( ring_rads_legend[k], ':' )
  if colon ne -1 then begin

    space = [ rstrpos( ring_rads_legend[k], ' ', colon ), $
              strpos( ring_rads_legend[k], ' ', colon ) ]
    if space[1] eq -1 then space[1] = strlen(ring_rads_legend[k])
    ll = float(strmid( ring_rads_legend[k], space[0]+1, colon-space[0]-1 ))
    mminusone = float(strmid( ring_rads_legend[k], colon+1, space[1]-colon-1 ))
    mm = mminusone + 1
    order = ll - mm + 1
    if strpos( ring_rads_legend[k], 'BW' ) ne -1 then order = -order

    if keyword_set(color) then clr=color else begin
      clr = get_mooncolor(ring_rads_legend[k],nocolor=1-mooncolor)
    endelse
    if keyword_set(_xr) then xr=_xr else xr=!x.crange
    if keyword_set(tkmradi) then begin
      xr=tkmradi
      if n_elements(sigma) eq n_elements(tkmradi) then varyingsigma = 1
    endif 

    if keyword_set(varyingsigma) then begin
      dr = tkmradi*1e3 - ring_rads[k]
      kconv = k_to_sigma(mm,ring_rads[k])
      ; k = kconv/sigma*dr
      oplot, xr, kconv/sigma*dr, l=2, co=clr
;      i = min(where( dr gt 0 ))
;      if i gt 0 then begin
;        kk[i] = kconv/sigma[i] * dr[i]
;        while i lt n_elements(tkmradi)-1 do begin
;          i = i + 1
;          kk[i] = kk[i-1] + kconv/sigma[i] * (dr[i]-dr[i-1])
;        endwhile
;      endif 
    endif else begin
      fit[1,j] = k_to_sigma(mm,ring_rads[k]) / sigma
      if order lt 0 then fit[1,j] = -fit[1,j]
      fit[0,j] = -fit[1,j]*ring_rads[k]
      xr1 = xr
      if keyword_set(ring_rads_maxlam) then begin
        if fit[1,j] gt 0 then begin
          xr1[1] = ( ring_rads_maxlam[k] - fit[0,j] )/fit[1,j]/1000
        endif else begin
          xr1[0] = ( ring_rads_maxlam[k] - fit[0,j] )/fit[1,j]/1000
        endelse 
      endif
      oplot, xr1, poly( xr1*1e3, fit[*,j] ), l=2, co=clr, thick=thick
    endelse 

  endif
endfor
if keyword_set(debug) then stop

end
