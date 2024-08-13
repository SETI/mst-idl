if keyword_set(noradscan) then begin
  print, 'Radial scan not plotted because noradscan=1.'
endif else if keyword_set(noplot) then begin
  print, 'Radial scan not plotted because noplot=1.'
endif else if not (keyword_set(radi) and keyword_set(val)) then begin

  print, 'Radial scan variables do not exist, cannot plot.'

endif else begin

  if keyword_set(clear) then begin
    radscan_xr = 0
    radscan_yr = 0
    clear = 0
  endif
  if keyword_set(radscan_xtit) then xtit=radscan_xtit else begin
    if keyword_set(phiout) then begin
      xtit = 'Longitude (deg)'
    endif else begin
      xtit = 'Radius'+tkmtit( radi, thoukm=thoukm )
    endelse
  endelse
  if not keyword_set(thoukm) then thoukm = 0
  if not keyword_set(radscan_np) then radscan_np = 0
  if n_elements(radscan_np) ne n_elements(val) or keyword_set(noradscan_np) then radscan_np = 0
  _errbar = get_errbar( errbar, radscan_np, val, noerrbar, sdmean )
  radscan_xr_base = [ min(tkm(radi,phi=phiout,thoukm=thoukm)), $
                      max(tkm(radi,phi=phiout,thoukm=thoukm)) ]
  if not keyword_set(radscan_xr) then radscan_xr = radscan_xr_base else begin
    radscan_xr[0] = radscan_xr[0] > radscan_xr_base[0]
    radscan_xr[1] = radscan_xr[1] < radscan_xr_base[1]
  endelse
  if not keyword_set(radscan_yr) then begin
    if radi[1] lt radi[0] then stop, 'Radi must be in ascending order.'
    x0 = max(where( tkm(radi,phi=phiout,thoukm=thoukm) le radscan_xr[0] ))
    if x0 eq -1 then x0 = 0
    x1 = min(where( tkm(radi,phi=phiout,thoukm=thoukm) ge radscan_xr[1] ))
    if x1 eq -1 then x1 = n_elements(radi)-1
    _radscan_yr = [ min(val[x0:x1])-mean(_errbar[where(finite(_errbar))]), max(val[x0:x1])+mean(_errbar[where(finite(_errbar))]) ]
    if keyword_set(radscan_ylog) and _radscan_yr[0] lt 0 then _radscan_yr[0] = min(val[x0:x1])
  endif else _radscan_yr = radscan_yr
  if not keyword_set(radscan_ps) then radscan_ps = 0
  if not keyword_set(radscan_tit) then radscan_tit = ''

  if !d.name eq 'X' and not keyword_set(radscan_keepwin) then begin
    if keyword_set(radscan_long) then xs = 1500 else xs = 640
    if keyword_set(radscan_double) then ys = 768 else ys = 512
    window, !d.window - (!d.window mod 2) + 2, xs=xs, ys=ys
  endif
  if keyword_set(radscan_ytit) then ytit=radscan_ytit else ytit='I/F'
  if keyword_set(radscan_xtn) then xtn=radscan_xtn else xtn=''
  if keyword_set(radscan_ytn) then ytn=radscan_ytn else ytn=''
  plot, tkm(radi,phi=phiout,thoukm=thoukm), val, /xs, /ys, $
        ytit=ytit, xtit=xtit, tit=radscan_tit, ylog=radscan_ylog, /nodata, $
        xtickn=xtn, ytickn=ytn, xr=radscan_xr, yr=_radscan_yr, $
        xtickfo=radscan_xtfo, ytickfo=radscan_ytfo, $
        xma=radscan_xma, yma=radscan_yma, noerase=radscan_noerase
  if total(_errbar) ne 0 then begin
    ;oplot, tkm(radi,phi=phiout,thoukm=thoukm), val-_errbar, l=1
    ;oplot, tkm(radi,phi=phiout,thoukm=thoukm), val+_errbar, l=1
    polyfill, [ tkm(radi,phi=phiout,thoukm=thoukm), tkm(reverse(radi), $
                                               phi=phiout,thoukm=thoukm) ], $
              [ val-_errbar, reverse(val+_errbar) ], noclip=0, color=gray()
  endif
  oplot, tkm(radi,phi=phiout,thoukm=thoukm), val, ps=radscan_ps

  if keyword_set(ring_rads) then for kk=0,n_elements(ring_rads)-1 do begin
    if keyword_set(radscan_ylog) then z0 = 1e-10 else z0 = -1e10
    if keyword_set(mooncolor) then begin
      clr=get_mooncolor(ring_rads_legend[kk])
      if ring_rads_legend[kk] eq '' then clr=get_mooncolor('Pan')
    endif else begin
      if !d.name eq 'X' then clr=white() else clr=0
    endelse
    oplot, tkm( ring_rads[[kk,kk]], thoukm=thoukm ), [z0,1e10], l=1, co=clr
    if keyword_set(smallrr) then chars = 1 else chars = !p.charsize
    if keyword_set(radscan_vert) then begin
      xyouts, tkm( ring_rads[kk], thoukm=thoukm ), $
	  _radscan_yr[0] + (_radscan_yr[1]-_radscan_yr[0])/100, $
	  ring_rads_legend[kk], orient=90, chars=chars, color=clr
    endif else begin
      xyouts, tkm( ring_rads[kk], thoukm=thoukm ), $
	  _radscan_yr[1] + (_radscan_yr[1]-_radscan_yr[0])/100, $
	  ring_rads_legend[kk], align=.5, chars=chars, color=clr
    endelse
  endfor

endelse

end
