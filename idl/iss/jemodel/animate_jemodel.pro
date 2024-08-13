; 29 August 2005

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

;tit = 'Time Since Reversal:  '
;yr=[-10,10]
;for j=0,7 do jemodel, 9, 7, time=200*j, tit=tit+strtrim(200*j,2), yr=yr
;for j=0,7 do jemodel, 9, 7, time=200*j, tit=tit+strtrim(200*j,2), yr=yr, jin=1

if keyword_set(dolzr) then begin
  if not keyword_set(psname) then psname='animate_jemodel'
  lzr, psname
  plot_color
  @plot_prepare
endif

tit = 'Number of J/E Libration Periods:  '
nn = 20.
yr = [-10,10]
for j=0.,nn-1 do begin
  t = (j/nn)*1461
  jemodel, 9, 7, time=t, tit=tit+string(t/1461,fo='(F4.2)'), yr=yr, $
           /zerophase, /nowrite
endfor
for j=0.,nn-1 do begin
  t = (j/nn)*1461
  jemodel, 9, 7, time=t, tit=tit+string(t/1461+1,fo='(F4.2)'), yr=yr, $
           /zerophase, /nowrite, /jin
endfor

if keyword_set(dolzr) then clzr

end
