dir='/home/borogove/iss/images/SOI/SOISPTURN'
phasej = [ 0.0d0, 0.0d0 ]
phasee = [ 0.0d0, 0.0d0 ]
dradi = 0
reverse = 0
sigma = 35.
xi_d = 0
minlam = 1.5
nr = 0
image_mid_time = ''
if keyword_set(qland) or keyword_set(hland) then wavelet_charsz=1 else wavelet_charsz=1.5
if keyword_set(hland) and keyword_set(dolzr) then !y.omargin=[0,15]

if keyword_set(findfile('jemodeldemo.sav')) then begin
  restore, 'jemodeldemo.sav'
endif else begin
  tit = 'Number of J/E Libration Periods:  '
  nn = 4.
  _rr = dblarr(2000,nn*2)
  _dw = dblarr(2000,nn*2)
  _dw2 = dblarr(2000,2,3,nn*2)
  yr = [-10,10]
  for j=0.,nn-1 do begin
    t = (j/nn)*1461
    jemodel, 9, 7, rr, dw, dw2=dw2, time=t, yr=yr, /zerophase, /nowrite, $
             tit=tit+string(t/1461,fo='(F4.2)'), decay=1, /usegreen, rres=rres
    _rr[*,j] = rr
    _dw[*,j] = dw
    _dw2[*,*,*,j] = dw2
  endfor
  for j=0.,nn-1 do begin
    t = (j/nn)*1461
    jemodel, 9, 7, rr, dw, dw2=dw2, time=t, yr=yr, /zerophase, /nowrite, /jin, $
             tit=tit+string(t/1461+1,fo='(F4.2)'), decay=1, /usegreen, rres=rres
    _rr[*,j+nn] = rr
    _dw[*,j+nn] = dw
    _dw2[*,*,*,j+nn] = dw2
  endfor
  save, nn, _rr, _dw, _dw2, filename='jemodeldemo.sav'
endelse

if not keyword_exists(cmyk) then cmyk = '_cmyk'
if not keyword_set(cmyk) then cmyk = ''
if not keyword_exists(aspect) then aspect = .4
if not keyword_exists(xax) then xax = 1
if keyword_set(dolzr) then begin
  if not keyword_set(psname) then psname='jemodeldemo'
  if keyword_set(aspect) then begin
    aspect = fix(aspect*10)/10.
    suff='_aspect'+string(aspect*10,fo='(I1)')
  endif else suff=''
  lzr, psname+suff+cmyk, aspect=aspect
  if keyword_set(cmyk) then device, /cmyk
  plot_color
  @plot_prepare
endif

!p.multi = 0
yy = 8
xx = max(_rr) - min(_rr)
_clr = [ ctred(), ctpurple(), ctblue(), ctgreen() ]
notn = replicate(' ',20)
x0 = 5
if keyword_set(xax) then begin
  xtn = [ '0', '50', '100', '150' ]
  xtv = tkm( min(_rr) + [ xtn - x0, xx + xtn ] )
  xtn = [ xtn, xtn ]
  xts = n_elements(xtv) - 1
  xtle = !p.ticklen
  yma = [4,1]
  xtit = 'Relative Distance (km)'
  y0 = 12
endif else begin
  xtn = notn
  xtv = 0
  xts = 0
  xtle = 1e-10
  yma = [1,1]
  xtit = ''
  y0 = 10
endelse
plot, tkm([min(_rr)-x0*2,max(_rr)+xx-15]), [-y0-(nn*2-1)*yy*2,17], /nodata, $
      /xs, /ys, xtickle=1e-10, ytickle=1e-10, xtickn=xtn, ytickn=notn, $
      xma=[1,1], yma=yma, xtickv=xtv, xticks=xts, xtit=xtit
axis, xaxis=0, xtickv=xtv, xtickn=notn, /data, xticks=xts
for m=0,nn*2-1 do begin
  if m ge 5 or m eq 0 then index = [1,0,1,3,2,3] else index = [0,1,0,2,3,2]
  clr = _clr[index]
  for j=0,1 do for k=0,2 do begin
    int = where( abs(_dw2[*,j,k,m]) gt 0.1, count )
    if count gt 0 then oplot, tkm( _rr[int,m] + xx ), $
                              _dw2[int,j,k,m] - yy*2*m, co=clr[3*j+k]
  endfor
  oplot, tkm( _rr[*,m] - x0 ), _dw[*,m] - yy*2*m
  xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])/40, -5 - yy*2*m, $
          't = '+strtrim(fix(m),2)+' yr'
endfor
for j=0,1 do for k=0,2 do begin
  oplot, tkm(rres[j,k]+xx)*[1,1], [-120,8], co=clr[3*j+k], l=1
endfor
xyouts, tkm( mean([max(_rr),min(_rr)]) ), !y.crange[1] - 5, align=.5, $
        'Model Result'
xyouts, tkm( mean([max(_rr)-15,min(_rr)]) + xx ), !y.crange[1] - 5, align=.5, $
        'Model Components'

if keyword_set(dolzr) then clzr

end
