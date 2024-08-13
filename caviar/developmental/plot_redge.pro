pro plot_redge_oplot, redge, index, tkmflag, thoukm, phi, ps, color, gaps

nr = (size(redge))[2]
if keyword_set(gaps) then begin
  dl = abs( redge[0,1:nr-1] - redge[0,0:nr-2] )
  gap = where( dl gt 100*median(dl), ngap )
  if ngap gt 0 then begin
    gap = [ 0l, gap+1, nr ]
  endif else gap = 0
endif
if not keyword_set(gap) then begin
  gap = [ 0l, nr ]
  ngap = 0l
endif
for j=0,ngap do oplot, redge[0,gap[j]:gap[j+1]-1], color=color, $
      tkm(redge[index,gap[j]:gap[j+1]-1],tkmflag,thoukm=thoukm,phi=phi), ps=ps

end

pro plot_redge, redge, redge_sigma, csz=csz, title=_title, oplot=oplot, keywords=keywords, xr=_xr, yr1=_yr1, yr2=_yr2, ps=ps, color=color, xtit=_xtit, ytit=_ytit, height=height, gaps=gaps, xma=_xma, yma=_yma, xtfo=xtfo, ytfo=ytfo, xticki=xti, yticki=yti, tkmflag=tkmflag, xtickn=xtickn, ytickn=ytickn, fullyr=fullyr

if n_params() eq 0 then begin
  print, 'Syntax:  PLOT_REDGE, redge, redge_sigma, /height'
  retall
endif

if keyword_set(_title) then title=_title else title=''
thoukm = get_thoukm(redge[1,*])
if not keyword_set(redge_sigma) then redge_sigma=redge*0
if keyword_set(_xtit) then xtit=_xtit else xtit='Longitude (deg)'
if keyword_set(_ytit) then yt=_ytit else yt='Radius'+tkmtit( redge[1,*], tkmflag, thoukm=thoukm )
redo:
if keyword_set(oplot) then begin
  index = 1
  plot_redge_oplot, redge, index, tkmflag, thoukm, phi, ps, color, gaps
endif else begin
  index = 1
  xt = xtit
  xtn = ''
  yma = [4,2]
  if keyword_set(_yr1) then yr=_yr1 else begin
    ;yr=[min(redge[index,*]-redge_sigma[index,*]),$
    ;    max(redge[index,*]+redge_sigma[index,*])]-thoukm
    if n_elements(redge[0,*]) ge 50 and not keyword_set(fullyr) then begin
      yr=tkm([min(smooth( redge[index,*] - redge_sigma[index,*], 50, /edge_truncate )),$
              max(smooth( redge[index,*] + redge_sigma[index,*], 50, /edge_truncate ))],$
              tkmflag, thoukm=thoukm)
    endif else begin
      yr=tkm([min( redge[index,*] - redge_sigma[index,*] ),$
              max( redge[index,*] + redge_sigma[index,*] )],$
              tkmflag, thoukm=thoukm)
    endelse
  endelse
  if keyword_set(_xr) then xr=_xr else begin
    xr=[ min(redge[0,*]), max(redge[0,*]) ]
  endelse
  ys = 9
  if keyword_set(height) then begin
    if height eq 1 then begin
      pmold = !p.multi
      !p.multi = [0,1,2]
      yma = [1,2]
      xt = ''
      xtn = replicate(' ',20)
    endif else begin
      index = 2
      yma = [4,-1]
      if keyword_set(_yr2) then yr=_yr2 else begin
        ;yr=[min(redge[index,*]-redge_sigma[index,*]),$
        ;    max(redge[index,*]+redge_sigma[index,*])]
        yr=[min(smooth(redge[index,*]-redge_sigma[index,*],50,/edge_truncate)),$
            max(smooth(redge[index,*]+redge_sigma[index,*],50,/edge_truncate))]
      endelse
      ys = 1
      yt = 'Gaussian Height'
      thoukm = 0
    endelse
  endif
  xma = [10,10]
  if not keyword_set(keywords) and n_elements(redge[*,0]) lt 4 then begin
    ; No info for right-hand y-axis
    ys = 1
    xma = [10,3]
  endif
  if keyword_set(_xma) then xma = _xma
  if keyword_set(_yma) then yma = _yma
  if not keyword_set(xtfo) then xtfo = ''
  if not keyword_set(ytfo) then ytfo = ''
  if not keyword_set(xti) then xti = 0
  if not keyword_set(yti) then yti = 0
  if keyword_set(xtickn) then  xtn = xtickn else xtn = ''
  if keyword_set(ytickn) then  ytn = ytickn else ytn = ''
  plot_nosci, redge[0,*], tkm(redge[index,*],tkmflag,thoukm=thoukm,phi=phi), $
      /xs, ys=ys, xtit=xt, ytit=yt, xma=xma, yma=yma, charsize=csz, $
      title=title, xr=xr, yr=yr, ps=ps, /nodata, xtickfo=xtfo, ytickfo=ytfo, $
      xticki=xti, yticki=yti, xtickn=xtn, ytickn=ytn
  plot_redge_oplot, redge, index, tkmflag, thoukm, phi, ps, color, gaps
endelse
oplot, redge[0,*], tkm( redge[index,*] + redge_sigma[index,*], tkmflag, thoukm=thoukm ), ps=3, color=color; l=1
oplot, redge[0,*], tkm( redge[index,*] - redge_sigma[index,*], tkmflag, thoukm=thoukm ), ps=3, color=color; l=1
if keyword_set(height) then if height gt 1 then begin
  !p.multi = pmold
  return
endif

ytit=''
savefile = '~/Data/caviar/developmental/tkmdefaultflag.sav'
if not keyword_set(tkmflag) then begin
  if keyword_set(findfile(savefile)) then restore, savefile else flag = 1
  tkmflag = flag
endif
if tkmflag eq 1 then fac=1e3 else fac=1
if keyword_set(keywords) then begin
  ytit = 'Radial Pixels'
  yr=(!y.crange-!y.crange[0])/keywords.ringplane_aimpoint_radial_scale[0]*fac
endif else if n_elements(redge[*,0]) ge 4 then begin
  ytit = 'Reprojected Pixels'
  ;yr=[min(redge[3,*]-redge_sigma[3,*]),max(redge[3,*]+redge_sigma[3,*])]
  ;yr=[0,max(redge[3,*]+redge_sigma[3,*])-min(redge[3,*]-redge_sigma[3,*])]
  yr=(!y.crange-!y.crange[0])/(redge[1,1]-redge[1,0])*(redge[3,1]-redge[3,0])*fac
endif
if not keyword_set(oplot) then if keyword_set(ytit) then begin
  axis, yaxis=1, /data, /ys, ytit=ytit, charsize=csz, yr=yr
endif

if keyword_set(height) then begin
  if height eq 1 then begin
    height = height + 1
    phi = 1
    title = ''
    goto, redo
  endif
endif

end
