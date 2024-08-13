dir = '035/AZSCNLOPH'
savedir = '035_azscnloph'
restore, savedir + '_inner.sav'
restore, savedir + '_outer.sav'

if not keyword_set(oldplot) then oldplot = 0
baserad = 133000.0d0
panrad = 133584.0d0

if keyword_set(dolzr) then begin
  lzr, savedir + '_plotpaul2', /port
  @plot_prepare
endif else window, xs=640, ys=960

mnlon = max([ min(lon_inner), min(lon_outer) ])
mxlon = min([ max(lon_inner), max(lon_outer) ])
nrsa = 200.
lonrsa = lindgen(nrsa) / (nrsa-1) * (mxlon-mnlon) + mnlon
smoothrad_outer = smooth( rad_outer + baserad, 10000, /edge )
smoothrad_inner = smooth( rad_inner + baserad, 10000, /edge )
radrsa = [ [interpol( smoothrad_outer, lon_outer, lonrsa )], $
           [interpol( smoothrad_inner, lon_inner, lonrsa )] ]
meanrad = rebin( radrsa, nrsa, 1 )

int = 15.
mnlonint = mnlon - (mnlon mod int); - int
mxlonint = mxlon - (mxlon mod int); + int
nint = ( mxlonint - mnlonint )/int

oldym = !y.margin
oldyom = !y.omargin
!y.margin = 0
!y.omargin = [4,2]
!p.multi = [0,1,6]
!p.charsize = 1.5

for k=0,1 do begin
  if k eq 0 then begin
    lon = lon_outer
    rad = rad_outer + baserad
    smoothrad = smoothrad_outer
    edge = 'Outer'
  endif else begin
    lon = lon_inner
    rad = rad_inner + baserad
    smoothrad = smoothrad_inner
    edge = 'Inner'
    if keyword_set(plotpaul1) then !y.margin = [4,-1]
  endelse 
  ytit = 'Radius - ' + string( mean(rad), fo='(I6)' ) + ' km'
  rad = rad - smoothrad
  yr = [ min(rad), max(rad) ]
  for j=0,nint-1 do begin
    if j mod !p.multi[2] eq 0 then tit = dir else tit = ''
    if (j+1) mod !p.multi[2] eq 0 then begin
      xtn = ''
      xtit = 'Longitude - L!D0!N (!Uo!N)'
    endif else begin
      xtn = replicate(' ',20)
      xtit = ''
    endelse 
    plot_nosci, lon - mnlonint - int*j, rad, /xs, /ys, xr=[0,int], yr=yr, $
                xtickn=xtn, xtit=xtit, ytit=ytit, tit=tit
    if j mod !p.multi[2] eq 0 then begin
      xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])/30, $
              !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, $
              'Encke Gap '+edge+' Edge'
    endif 
    xyouts, !x.crange[1] - (!x.crange[1]-!x.crange[0])/30, $
            !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, $
            'L!D0!N = '+strtrim( fix(mnlonint+int*j), 2 )+'!Uo!N', align=1
    if !d.name eq 'X' then if (j+1) mod !p.multi[2] eq 0 then stop
  endfor
endfor
if keyword_set(dolzr) then clzr

!y.margin = oldym
!y.omargin = oldyom

end

