;restore, '$DATA/iss/images/bharris/046/RDHRESSCN/ring_rads_index.sav'
if not keyword_exists(mooncolor) then mooncolor = 1
if keyword_set(dolzr) then begin
  today = current_time_6digit()
  dolzr, 'restorque_plot_'+today
  @plot_prepare
  plot_color
endif else device, decomposed=0
if not keyword_exists(cmyk) then cmyk=1
if !d.name eq 'PS' then device, cmyk=cmyk

torq = 0.0d0
rres = 0.0d0
rname = ''
series = 0
mm = 0l
kk = 0l
pp = 0l
qq = 0l
bb = 0l

if not keyword_exists(lc82) then lc82 = 0;1
if keyword_set(lc82) then lc82plot = 1
; If harmonics are separated by more than this, include both
if not keyword_exists(harmdist) then harmdist = 10	
if not keyword_exists(harmdist2) then harmdist2 = 50	

; Still to do:  
; 1) Pan OLRs
; 2) third-order waves (like Mimas), and higher order,
; 3) 1:0 resonances
; 4) Bending waves, and e*I'^2 density waves
; Most of these will involve redoing the derivation myself, and seeing where
; to put the necessary Disturbing Function components.  
; For higher-order waves, need to update Laplace coefficient program.

for m=2,100 do begin
  ; Atlas first order
  torq = [ torq, restorque(m,m-1,615,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 61510 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 0 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
endfor
for m=2,200 do begin
  ; Atlas second order
  torq = [ torq, restorque(m+1,m-1,615,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 61520 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
endfor
;for m=20,50 do begin
;  ; Atlas exterior, first order
;  torq = [ torq, restorque(m,m+1,615,res_descrip=res_descrip,/short,$
;                           lc82=lc82,rres=_rres) ]
;  rres = [ rres, _rres ]
;  series = [ series, 61511 ]
;  rname = [ rname, res_descrip ]
;  mm = [ mm, m ]
;endfor
for m=2,92 do begin
  ; Pan first order
  torq = [ torq, restorque(m,m-1,618,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 61810 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 0 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
endfor
;for m=2,9 do begin
;  ; Pan second order
;  torq = [ torq, restorque(m+1,m-1,618,res_descrip=res_descrip,/short,$
;                           lc82=lc82,rres=_rres) ]
;  rres = [ rres, _rres ]
;  series = [ series, 61820 ]
;  rname = [ rname, res_descrip ]
;  mm = [ mm, m ]
;endfor
for m=10,92 do begin
  ; Pan exterior, first order
  torq = [ torq, restorque(m,m+1,618,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 61811 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 0 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
endfor
for m=2,38 do begin
  ; Prometheus first order
  torq = [ torq, restorque(m,m-1,616,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 61610 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 0 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
endfor
for m=2,74 do begin
  ; Prometheus second order
  if ( (m-1) mod 2 ) ne 0 then good=1 else begin
    if abs( resloc(m+1,m-1,616) - $
            resloc((m+1)/2,(m-1)/2,616) ) gt harmdist then good=1 else good=0
  endelse
  if good then begin
    torq = [ torq, restorque(m+1,m-1,616,res_descrip=res_descrip,/short,$
                             lc82=lc82,rres=_rres) ]
    rres = [ rres, _rres ]
    series = [ series, 61620 ]
    rname = [ rname, res_descrip ]
    mm = [ mm, m ]
    kk = [ kk, 1 ]
    pp = [ pp, 0 ]
    qq = [ qq, 1 ]
    bb = [ bb, 0 ]
  endif
endfor
for m=4,105 do begin
  ; Prometheus third order
  if ( (m-1) mod 3 ) ne 0 then good=1 else begin
    if abs( resloc(m+2,m-1,616) - $
            resloc((m+2)/3,(m-1)/3,616) ) gt harmdist then good=1 else good=0
  endelse
  if good then begin
    torq = [ torq, restorque(m+2,m-1,616,res_descrip=res_descrip,/short,$
                             lc82=lc82,rres=_rres) ]
    rres = [ rres, _rres ]
    series = [ series, 61630 ]
    rname = [ rname, res_descrip ]
    mm = [ mm, m ]
    kk = [ kk, 2 ]
    pp = [ pp, 0 ]
    qq = [ qq, 1 ]
    bb = [ bb, 0 ]
  endif
endfor
for m=2,19 do begin
  ; Pandora first order
  torq = [ torq, restorque(m,m-1,617,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 61710 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 0 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
endfor
for m=2,38 do begin
  ; Pandora second order
  if ( (m-1) mod 2 ) ne 0 then good=1 else begin
    if abs( resloc(m+1,m-1,617) - $
            resloc((m+1)/2,(m-1)/2,617) ) gt harmdist then good=1 else good=0
  endelse
  if good then begin
    torq = [ torq, restorque(m+1,m-1,617,res_descrip=res_descrip,/short,$
                             lc82=lc82,rres=_rres) ]
    rres = [ rres, _rres ]
    series = [ series, 61720 ]
    rname = [ rname, res_descrip ]
    mm = [ mm, m ]
    kk = [ kk, 1 ]
    pp = [ pp, 0 ]
    qq = [ qq, 1 ]
    bb = [ bb, 0 ]
  endif
endfor
for m=3,55 do begin
  ; Pandora third order
  if ( (m-1) mod 3 ) ne 0 then good=1 else begin
    if abs( resloc(m+2,m-1,617) - $
            resloc((m+2)/3,(m-1)/3,617) ) gt harmdist then good=1 else good=0
  endelse
  if good then begin
    torq = [ torq, restorque(m+2,m-1,617,res_descrip=res_descrip,/short,$
                             lc82=lc82,rres=_rres) ]
    rres = [ rres, _rres ]
    series = [ series, 61730 ]
    rname = [ rname, res_descrip ]
    mm = [ mm, m ]
    kk = [ kk, 2 ]
    pp = [ pp, 0 ]
    qq = [ qq, 1 ]
    bb = [ bb, 0 ]
  endif
endfor
for m=2,10 do begin
  ; Janus first order
  torq = [ torq, restorque(m,m-1,610,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 61010 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 0 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
endfor
for m=2,21 do begin
  ; Janus second order
  if ( (m-1) mod 2 ) ne 0 then good=1 else begin
    if abs( resloc(m+1,m-1,610) - $
            resloc((m+1)/2,(m-1)/2,610) ) gt harmdist2 then good=1 else good=0
  endelse
  if good then begin
    torq = [ torq, restorque(m+1,m-1,610,res_descrip=res_descrip,/short,$
                             lc82=lc82,rres=_rres) ]
    rres = [ rres, _rres ]
    series = [ series, 61020 ]
    rname = [ rname, res_descrip ]
    mm = [ mm, m ]
    kk = [ kk, 1 ]
    pp = [ pp, 0 ]
    qq = [ qq, 1 ]
    bb = [ bb, 0 ]
  endif
endfor
for m=3,29 do begin
  ; Janus third order
  if ( (m-1) mod 3 ) ne 0 then good=1 else begin
    if abs( resloc(m+2,m-1,610) - $
            resloc((m+2)/3,(m-1)/3,610) ) gt harmdist2 then good=1 else good=0
  endelse
  if good then begin
    torq = [ torq, restorque(m+2,m-1,610,res_descrip=res_descrip,/short,$
                             lc82=lc82,rres=_rres) ]
    rres = [ rres, _rres ]
    series = [ series, 61030 ]
    rname = [ rname, res_descrip ]
    mm = [ mm, m ]
    kk = [ kk, 2 ]
    pp = [ pp, 0 ]
    qq = [ qq, 1 ]
    bb = [ bb, 0 ]
  endif
endfor
for m=4,37 do begin
  ; Janus fourth order
  if ( (m-1) mod 2 ) ne 0 then good=1 else begin
    if abs( resloc(m+3,m-1,610) - $
            resloc((m+3)/2,(m-1)/2,610) ) gt harmdist2 then good=1 else good=0
  endelse
  if good then begin
    torq = [ torq, restorque(m+3,m-1,610,res_descrip=res_descrip,/short,$
                             lc82=lc82,rres=_rres) ]
    rres = [ rres, _rres ]
    series = [ series, 61040 ]
    rname = [ rname, res_descrip ]
    mm = [ mm, m ]
    kk = [ kk, 3 ]
    pp = [ pp, 0 ]
    qq = [ qq, 1 ]
    bb = [ bb, 0 ]
  endif
endfor
for m=2,10 do begin
  ; Epimetheus first order
  torq = [ torq, restorque(m,m-1,611,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 61110 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 0 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
endfor
for m=2,21 do begin
  ; Epimetheus second order
  if ( (m-1) mod 2 ) ne 0 then good=1 else begin
    if abs( resloc(m+1,m-1,611) - $
            resloc((m+1)/2,(m-1)/2,611) ) gt harmdist2 then good=1 else good=0
  endelse
  if good then begin
    torq = [ torq, restorque(m+1,m-1,611,res_descrip=res_descrip,/short,$
                             lc82=lc82,rres=_rres) ]
    rres = [ rres, _rres ]
    series = [ series, 61120 ]
    rname = [ rname, res_descrip ]
    mm = [ mm, m ]
    kk = [ kk, 1 ]
    pp = [ pp, 0 ]
    qq = [ qq, 1 ]
    bb = [ bb, 0 ]
  endif
endfor
for m=3,29 do begin
  ; Epimetheus third order
  if ( (m-1) mod 3 ) ne 0 then good=1 else begin
    if abs( resloc(m+2,m-1,611) - $
            resloc((m+2)/3,(m-1)/3,611) ) gt harmdist2 then good=1 else good=0
  endelse
  if good then begin
    torq = [ torq, restorque(m+2,m-1,3,res_descrip=res_descrip,/short,$
                             lc82=lc82,rres=_rres) ]
    rres = [ rres, _rres ]
    series = [ series, 61130 ]
    rname = [ rname, res_descrip ]
    mm = [ mm, m ]
    kk = [ kk, 2 ]
    pp = [ pp, 0 ]
    qq = [ qq, 1 ]
    bb = [ bb, 0 ]
  endif
endfor
for m=4,37 do begin
  ; Epimetheus fourth order
  if ( (m-1) mod 2 ) ne 0 then good=1 else begin
    if abs( resloc(m+3,m-1,611) - $
            resloc((m+3)/2,(m-1)/2,611) ) gt harmdist2 then good=1 else good=0
  endelse
  if good then begin
    torq = [ torq, restorque(m+3,m-1,611,res_descrip=res_descrip,/short,$
                             lc82=lc82,rres=_rres) ]
    rres = [ rres, _rres ]
    series = [ series, 61140 ]
    rname = [ rname, res_descrip ]
    mm = [ mm, m ]
    kk = [ kk, 3 ]
    pp = [ pp, 0 ]
    qq = [ qq, 1 ]
    bb = [ bb, 0 ]
  endif
endfor
for m=2,3 do begin
  ; Mimas first order
  torq = [ torq, restorque(m,m-1,601,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 60110 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 0 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
endfor
for m=2,5 do begin
  ; Mimas second order
  torq = [ torq, restorque(m+1,m-1,601,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 60120 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 1 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
;  torq = [ torq, restorque(m+1,m-1,601,res_descrip=res_descrip,/short,$
;                           lc82=lc82,rres=_rres,/bending) ]
;  rres = [ rres, _rres ]
;  series = [ series, 60121 ]
;  rname = [ rname, res_descrip ]
;  mm = [ mm, m ]
;  kk = [ kk, 0 ]
;  pp = [ pp, 1 ]
;  qq = [ qq, 0 ]
;  bb = [ bb, 1 ]
endfor
for m=2,7 do begin
  ; Mimas third order
  torq = [ torq, restorque(m+2,m-1,601,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 60130 ]
  if keyword_set(lc82plot) and not keyword_set(lc82) then begin
    rname = [ rname, res_descrip ]
  endif else begin
    ;rname = [ rname, res_descrip + ' (ee''^2)' ]
    rname = [ rname, res_descrip ]
  endelse
  mm = [ mm, m ]
  kk = [ kk, 2 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
;  torq = [ torq, restorque(m+2,m-1,601,res_descrip=res_descrip,/short,$
;                           lc82=lc82,rres=_rres,/bending) ]
;  rres = [ rres, _rres ]
;  series = [ series, 60131 ]
;  ;rname = [ rname, res_descrip + ' (e''II'')' ]
;  rname = [ rname, res_descrip ]
;  mm = [ mm, m ]
;  kk = [ kk, 1 ]
;  pp = [ pp, 1 ]
;  qq = [ qq, 0 ]
;  bb = [ bb, 1 ]
;  torq = [ torq, restorque(m+2,m-1,601,res_descrip=res_descrip,/short,$
;                           lc82=lc82,rres=_rres, pp=2) ]
;  rres = [ rres, _rres ]
;  series = [ series, 60132 ]
;  ;rname = [ rname, res_descrip + ' (eI''^2)' ]
;  rname = [ rname, res_descrip ]
;  mm = [ mm, m ]
;  kk = [ kk, 0 ]
;  pp = [ pp, 2 ]
;  qq = [ qq, 1 ]
;  bb = [ bb, 0 ]
endfor
for m=2,7 do begin
  ; Mimas fourth order
  torq = [ torq, restorque(m+3,m-1,601,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 60140 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 3 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
endfor
for m=3,10 do begin
  ; Mimas fifth order
  torq = [ torq, restorque(m+4,m-1,601,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 60150 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 4 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
endfor
for m=2,2 do begin
  ; Enceladus 3:1
  torq = [ torq, restorque(m+1,m-1,602,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 60220 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 1 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
endfor
for m=2,3 do begin
  ; Enceladus third order
  torq = [ torq, restorque(m+2,m-1,602,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 60230 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
  kk = [ kk, 2 ]
  pp = [ pp, 0 ]
  qq = [ qq, 1 ]
  bb = [ bb, 0 ]
endfor
for m=2,3 do begin
  ; Enceladus fourth order
  torq = [ torq, restorque(m+3,m-1,602,res_descrip=res_descrip,/short,$
                           lc82=lc82,rres=_rres) ]
  rres = [ rres, _rres ]
  series = [ series, 60240 ]
  rname = [ rname, res_descrip ]
  mm = [ mm, m ]
endfor
;for m=1,1 do begin
;  ; Iapetus 1:0
;  torq = [ torq, restorque(m,m-1,608,res_descrip=res_descrip,/short,$
;                           lc82=lc82,rres=_rres) ]
;  rres = [ rres, _rres ]
;  series = [ series, 60802 ]
;  rname = [ rname, res_descrip ]
;  mm = [ mm, m ]
;  order = [ order, 1 ]
;  ; Iapetus -1:0 BW
;  torq = [ torq, restorque(-m,m-1,608,res_descrip=res_descrip,/short,$
;                           lc82=lc82,rres=_rres,bending=1) ]
;  rres = [ rres, _rres ]
;  series = [ series, 60803 ]
;  rname = [ rname, res_descrip ]
;  mm = [ mm, m ]
;  order = [ order, 1 ]
;endfor
;for m=1,1 do begin
;  ; Titan 1:0
;  torq = [ torq, restorque(m,m-1,606,res_descrip=res_descrip,/short,$
;                           lc82=lc82,rres=_rres) ]
;  rres = [ rres, _rres ]
;  series = [ series, 60602 ]
;  rname = [ rname, res_descrip ]
;  mm = [ mm, m ]
;  order = [ order, 1 ]
;  ; Titan -1:0 BW
;  torq = [ torq, restorque(-m,m-1,606,res_descrip=res_descrip,/short,$
;                           lc82=lc82,rres=_rres,bending=1) ]
;  rres = [ rres, _rres ]
;  series = [ series, 60603 ]
;  rname = [ rname, res_descrip ]
;  mm = [ mm, m ]
;  order = [ order, 1 ]
;endfor

; For apsidal resonance, not yet implemented in restorque.pro, 
; enter values from LC82
if keyword_set(lc82) then begin
  torq = [ torq, -1.39e18, -7.17e12 ]
endif else begin
  torq = [ torq, -1.39e18/1e20, -7.17e12/1e20 ]
endelse
rres = [ rres, resloc(1,0,606,res_descrip=res_descrip1,/short,lc82=lc82), $
               resloc(1,0,608,res_descrip=res_descrip2,/short,lc82=lc82) ]
series = [ series, 60601, 60801 ]
rname = [ rname, res_descrip1, res_descrip2 ]
mm = [ mm, 1 ]
kk = [ kk, 0 ]
pp = [ pp, 0 ]
qq = [ qq, 1 ]
bb = [ bb, 0 ]

torq = clip(torq)
rres = clip(rres)
rname = clip(rname)
mm = clip(mm)
kk = clip(kk)
pp = clip(pp)
qq = clip(qq)
bb = clip(bb)

if keyword_set(print_ccp) then begin
  for j=0,1 do begin
    foo = where( strmid(rname,0,2) eq (['Mi','Ja'])[j], count )
    for kkk=0,count-1 do begin
      k = foo[kkk]
      spacing = ' '
      for kkkk=0,16-strlen(rname[k]) do spacing = spacing + ' '
      print, rname[k] + spacing + $
             string(mm[k]-qq[k]-bb[k],fo='(I3)') + $
             string(qq[k],fo='(I3)') + string(bb[k],fo='(I3)') + $
             string(-mm[k]-kk[k]-pp[k],fo='(I4)') + string(kk[k],fo='(I3)') + $
             string(pp[k],fo='(I3)') + string(rres[k]/60330,fo='(F10.5)') + $
             string(rres[k],fo='(F12.2)') + string(torq[k],fo='(E15.5)')
    endfor 
  endfor
endif

if keyword_set(noplot) then retall

series = clip(series)
solid_circles
if !d.name eq 'X' then syms=1 else syms=1;0.5
if keyword_set(lc82plot) then begin
  moons = [ 'Janus', 'Prometheus', 'Epimetheus', 'Pandora' ]
  moons1 = 'Atlas'
  if not keyword_set(lc82) then begin
    lc82 = 0
    rres = rres * 1e5
    torq = torq * 1e20
    yr = [15.4,18.6]
    moons = [ moons, 'Mimas' ]
    moont = [ 17.9, 16.45, 16.8, 16.05, 17.6 ]
    moons1 = [ moons1, 'Mimas', 'Janus' ]
    moont1x = [ 2.265, 2.19, 2.17 ]
    moont1y = [ 15.6, 15.53, 15.77 ]
    tit = ''
  endif else begin
    yr = [15.75,19.3]
    moont = [ 18.55, 17.55, 17.3, 16.95 ]
    moont1x = 2.26
    moont1y = 16
    tit='Lissauer and Cuzzi (1982) Values'
  endelse
  plot, [2.07,2.27], yr, /xs, /ys, /nodata, tit=tit, $
    xtit='Radial Position (R!DSaturn!N)', $
    ytit='Log!D10!N Resonance Strength (T!UL!N!Dl,m!N/!Ms, cm!U4!N sec!U-2!N)'
  use = [61510,61710,61610,61010,61110,60120]
  if lc82 eq 0 then use = [ use, 61020, 60130 ]
  for j=0,n_elements(use)-1 do begin
    foo = where( series eq use[j] and rres/60330e5 lt 2.2675, count )
    if lc82 eq 0 and use[j] eq 61020 then begin
      foo = foo[indgen(count/2)*2]
      count = n_elements(foo)
    endif 
    if count gt 0 then begin
      oplot, rres[foo]/60330e5,  alog10(abs(torq[foo])), ps=-8, syms=syms
      case use[j] of
        61510: if lc82 then _foo = foo[0] else _foo = foo[[38,98]]
        61710: if lc82 then _foo = foo[12:count-1] else _foo = foo[0]
        61610: if lc82 then _foo = foo[[8,13,18]] else _foo= foo[[13,18,23,28,33]]
        61010: _foo = foo[0:5]
        61110: if lc82 then _foo = foo[3:4] else _foo = foo[[2,3,5]]
        60130: _foo = foo[[9,12]]
        else: _foo = foo
      endcase
      align = 0
      mark:
      xyouts, rres[_foo]/60330e5, alog10(abs(torq[_foo])), $
              '  '+strmid(rname[_foo],2,20)+'    ', orient=90, align=align
      if align eq 0 then begin
        case use[j] of 
          61710: if lc82 then _foo = foo[4:11] else _foo = foo[4:count-1]
          61610: if lc82 then _foo = foo[0] else _foo = foo[8]
          61110: if lc82 then _foo = foo[[2,5]] else _foo = foo[4]
          else: _foo = foo[0]
        endcase
        align = 1
        goto, mark
      endif
    endif
  endfor
  for j=0,n_elements(moons)-1 do begin
    xyouts, 2.09, moont[j], moons[j]
  endfor
  xyouts, moont1x, moont1y, moons1, align=1
endif else begin
  if keyword_set(_ring_rads_index) then begin
    restore, '~/idl/iss/wavelet/ring_rads_index_combine.pro'
    if n_elements(_ring_rads_index) ne n_elements(_ring_rads) then stop
  endif 
  xtit='Radius'+tkmtit()
  ytit='Log!D10!N Resonance Strength (T!UL!N!Dl,m!N/!Ms, km!U4!N sec!U-2!N)'
  if keyword_set(aonly) then begin
    plot, [122,137], [-8.1,-1.5], /xs, /ys, /nodata, xtit=xtit, ytit=ytit
    rads = [ 122.34, 133.423, 133.745, 136.5, 136.78 ]
    ints1 = rads
    intnames = replicate('',5)
  endif else begin
    rads = [ 66.9, 74.658, 91.975, 117.507, 122.34, 136.78, 140.219 ]
    ints1 = rebin( [ [rads[1:5]], [rads[0:4]] ], 5, 1 )
    ints1 = [ ints1, rads[6] ]
    intnames = [ 'D', 'C', 'B', 'CD', 'A', 'F' ]
    if keyword_set(xlog) then begin
      xlog0 = 145
      xr = [60,137.5]
      xtn = indgen( (xr[1]-xr[0])/10+1 )*10 + 60
      xtn1 = indgen( (xr[1]-xr[0])/2+1 )*2 + 60
      xtnb = replicate(' ',60)
      plot, xlog0-xr, [-11.5,0.5], /xs, /ys, /nodata, $
            xtit=xtit, ytit=ytit, /xlog, xr=xlog0-xr, $
            xtickv=xlog0-xtn, xticks=n_elements(xtn)-1, xtickn=strtrim(xtn,2)
      axis, xaxis=0, /data, /xs, xtickle=!p.ticklen/2, $
            xtickv=xlog0-xtn1, xticks=n_elements(xtn1)-1, xtickn=xtnb
      axis, xaxis=1, /data, /xs, xtickle=!p.ticklen/2, $
            xtickv=xlog0-xtn1, xticks=n_elements(xtn1)-1, xtickn=xtnb
      rads = xlog0 - rads
      torq = vec_remove( torq, where(rres gt xr[1]*1e3) )
      rname = vec_remove( rname, where(rres gt xr[1]*1e3) )
      series = vec_remove( series, where(rres gt xr[1]*1e3) )
      rres = vec_remove( rres, where(rres gt xr[1]*1e3) )
      rres = xlog0*1e3 - rres
      ints1 = rebin( [ [alog10(rads[1:5])], [alog10(rads[0:4])] ], 5, 1 )
      ints1 = [ 10^ints1, rads[6] ]
    endif else begin
      ;plot, [60,144], [-11.5,0.5], /xs, /ys, /nodata, xtit=xtit, ytit=ytit
      plot, [60,144], [-14,0.5], /xs, /ys, /nodata, xtit=xtit, ytit=ytit
    endelse
  endelse
  use = series[uniq( series, sort(series) )]
  if keyword_set(aonly) then begin
    foo = where( use eq 61040 or use eq 61140 or use eq 60150, count )
    if count gt 0 then use = vec_remove( use, foo )
  endif 
  for j=0,n_elements(use)-1 do begin
    foo = where( series eq use[j], count )
    if count gt 0 then begin
      if keyword_set(_ring_rads_index) then begin
        ring_rads_index = fltarr(count)
        for q=0,count-1 do begin
          foo1 = where( strmid(_ring_rads_legend,0,strlen(rname[foo[q]])) eq $
                        rname[foo[q]] and $
                        strmid(_ring_rads_legend,strlen(rname[foo[q]]),3) ne $
                        ' BW', count )
          if count eq 0 then begin
            if strmid(rname[foo[q]],0,2) eq 'Ja' or $
               strmid(rname[foo[q]],0,2) eq 'Ep' or $
               strmid(rname[foo[q]],0,2) eq 'Pr' or $
               strmid(rname[foo[q]],0,2) eq 'Pd' then begin
              ;print, rname[foo[q]] ; These are harmonics of stronger resonances
              ring_rads_index[q] = 5
            endif else stop
          endif else if count eq 1 then begin
            ring_rads_index[q] = _ring_rads_index[foo1]
;          endif else if min(_ring_rads_index[foo1]) eq $
;                        max(_ring_rads_index[foo1]) then begin
;            ring_rads_index[q] = _ring_rads_index[foo1[0]]
;          endif else stop
          endif else ring_rads_index[q] = min(_ring_rads_index[foo1])
        endfor 
      endif 
      if not keyword_set(mooncolor) then mooncolor=0
      clr = get_mooncolor( rname[foo[0]], nocolor=1-mooncolor )
      oplot, tkm(rres[foo]), alog10(abs(torq[foo])), color=clr
      for rri=1,5 do begin
        case rri of
          1:  begin
            solid_circles
            ps=8
          end 
          2:  begin
            open_circles
            ps=8
          end 
          3:  ps=3
          4:  ps=3
          5:  ps=1
        endcase 
        if keyword_set(ring_rads_index) then begin
          foo1 = where( ring_rads_index eq rri, count )
       endif else foo1 = indgen(count)
        if count gt 0 then oplot, tkm(rres[foo[foo1]]), $
                                  alog10(abs(torq[foo[foo1]])), $
                                  ps=ps, color=clr, syms=syms
      endfor 
      if not keyword_set(aonly) then begin
        if rname[foo[0]] eq 'Pr 3:1' then align=0 else align=1
        if rname[foo[0]] eq 'Ia 1:0' then ia=0 else ia=1
        xyouts, tkm(rres[foo[0]]), alog10(abs(torq[foo[0]]))-.2*ia, $
                rname[foo[0]], align=align, color=clr
      endif
    endif
  endfor
  if keyword_set(aonly) then begin
    rnplot = [ 'Pr 12:10', 'Pd 11:9', 'Pan 8:7', 'At 7:6', $
               'Pd 5:4', 'Pr 6:5', 'Ep 4:3', 'Ja 9:7', 'Ep 9:7', $
               'Ja 4:3', 'Mi 5:3', '!CPan 30:31  ', '!CMi 7:4   ', 'Mi 9:5', $
               'Ja 11:8', 'Ep 11:8', '!CPr 37:34   ', 'Pd 28:25', $
               '!CAt 36:34', 'En 5:2' ]
    rplot = dblarr(n_elements(rnplot))
    for j=0,n_elements(rnplot)-1 do begin
      _rnplot = rnplot[j]
      if strmid(_rnplot,0,2) eq '!C' then _rnplot = strmid(_rnplot,2,100)
      _rnplot = strtrim( _rnplot, 2 )
      foo = where( rname eq _rnplot, count )
      if ( strmid(_rnplot,0,3) eq 'Pan' and _rnplot ne 'Pan 8:7' ) or $
        _rnplot eq 'Pd 11:9' or _rnplot eq 'Pr 12:10' or _rnplot eq 'Pd 28:25' or _rnplot eq 'Pr 37:34' or _rnplot eq 'Mi 7:4' then align=1 else align=0
      xyouts, tkm(rres[foo])+.1, alog10(abs(torq[foo]))+.05, rnplot[j], $
              align=align, chars=1
    endfor
  endif
  for j=0,n_elements(rads)-1 do oplot, rads[j]*[1,1], [-1e10,1e10], l=1
  ;xyouts, ints1, replicate(-11,n_elements(ints1)), intnames, align=.5
  xyouts, ints1, replicate(-13.5,n_elements(ints1)), intnames, align=.5
endelse

if keyword_set(dolzr) then clzr

end
