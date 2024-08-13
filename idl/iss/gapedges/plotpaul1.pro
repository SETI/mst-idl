savedir = '035_azscnloph'
restore, savedir + '_inner.sav'
restore, savedir + '_outer.sav'

if not keyword_set(oldplot) then oldplot = 0
baserad = 133000.0d0
panrad = 133584.0d0

if keyword_set(dolzr) then begin
  lzr, savedir + '_plotpaul1', /port
  @plot_prepare
  plot_color
  if not keyword_exists(plotpaul1) then plotpaul1 = 1
endif

if oldplot eq 1 then begin
  plot_nosci, lon_inner, rad_inner+baserad, /xs, /ys, $
              yr=[ min(rad_inner), max(rad_outer) ]+baserad, $
              xtit='Longitude (!Uo!N)', ytit='Radius (km)', $
              tit='Encke Gap to Scale'
  oplot, lon_outer, rad_outer+baserad
endif

mnlon = max([ min(lon_inner), min(lon_outer) ])
mxlon = min([ max(lon_inner), max(lon_outer) ])
nrsa = 200.
lonrsa = lindgen(nrsa) / (nrsa-1) * (mxlon-mnlon) + mnlon
smoothrad_outer = smooth( rad_outer + baserad, 10000, /edge )
smoothrad_inner = smooth( rad_inner + baserad, 10000, /edge )
radrsa = [ [interpol( smoothrad_outer, lon_outer, lonrsa )], $
           [interpol( smoothrad_inner, lon_inner, lonrsa )] ]
meanrad = rebin( radrsa, nrsa, 1 )

oldym = !y.margin
oldyom = !y.omargin
!y.margin = 0
!y.omargin = [4,2]
if oldplot eq 3 then begin
  !p.multi = [0,1,3]
  !p.charsize = 1.5
endif else !p.multi = [0,1,2]
if keyword_set(plotpaul1) then begin
  !p.multi = [0,1,4]
  !p.charsize = 1.5
  !y.omargin = 0
  oldplot = 2
  next:
  !y.margin = [1,2]
endif
for k=0,1 do begin
  if k eq 0 then begin
    lon = lon_outer
    rad = rad_outer + baserad
    smoothrad = smoothrad_outer
    edge = 'Outer'
    xtn = replicate(' ',20)
    xtit = ''
  endif else begin
    lon = lon_inner
    rad = rad_inner + baserad
    smoothrad = smoothrad_inner
    edge = 'Inner'
    xtn = ''
    xtit = 'Longitude (!Uo!N)'
    if keyword_set(plotpaul1) then !y.margin = [4,-1]
  endelse 
  if oldplot eq 0 then begin
    ytit = 'Radius - ' + string( mean(rad), fo='(I6)' ) + ' km'
    rad = rad - smoothrad
  endif else ytit = 'Radius (km)'
  if oldplot eq 3 then begin
    lon = lonrsa
    rad = radrsa[*,k]
    if k eq 0 then plot_nosci, lon, meanrad, /xs, /ys, xtit='Mean Radius (km)'
  endif
  if oldplot eq 4 then begin
    lon = lonrsa
    rad = radrsa[*,k] - meanrad + panrad
  endif
  if oldplot eq 5 then rad = rad - interpol( meanrad, lonrsa, lon ) + panrad
  plot_nosci, lon, rad, /xs, /ys, $
              xtickn=xtn, xtit=xtit, ytit=ytit
  xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])/30, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, $
          'Encke Gap '+edge+' Edge'
  if oldplot eq 2 then oplot, lon, smoothrad, co=ctred()
endfor
if keyword_set(plotpaul1) and oldplot eq 2 then begin
  oldplot = 0
  goto, next
endif
if keyword_set(dolzr) then clzr

!y.margin = oldym
!y.omargin = oldyom

end

