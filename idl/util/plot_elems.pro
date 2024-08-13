pro plot_elems, _elems, time=_time, tunits=_tunits, aunits=_aunits, tkm=_tkm, $
                tit=_tit, nocolor=nocolor, ps=_ps, radians=radians, noq=noq, $
                trendln=trendln, trendap=trendap, interv=interv

yoma = !y.omargin
!y.omargin = [4,2]
pm = !p.multi
!p.multi = [0,1,3]

elems = _elems
sz = size(elems)
if keyword_set(_aunits) then begin
  aunits = _aunits
  if strmid(aunits,0,2) ne ' (' then aunits = ' ('+aunits+')'
endif else begin
  if keyword_set(_tkm) then aunits=tkmtit() else aunits=''
endelse
if keyword_set(_tunits) then begin
  tunits = _tunits
  if strmid(tunits,0,2) ne ' (' then tunits = ' ('+tunits+')'
endif else tunits=''
if keyword_set(_time) then begin
  time = _time 
  xtit = 'Time'+tunits
endif else begin
  time = lindgen(sz[1])
  xtit = ''
endelse
if keyword_set(_tkm) then elems[*,0] = tkm(elems[*,0])
if keyword_set(_tit) then tit=_tit else tit=''
if keyword_set(nocolor) then begin
  l = 1
  if !d.name eq 'PS' then clr=0 else clr=ctwhite()
  cn = 'dotted'
endif else begin
  l = 0
  clr = ctgreen()
  cn = 'green'
endelse
if keyword_set(_ps) then ps=_ps else ps=0
if keyword_set(radians) then begin
  angmax = 2*!pi
  angunits = ' (rad)'
endif else begin
  angmax = 360
  angunits = ' (deg)'
endelse
if n_elements(time) lt sz[1] then begin
  elems = elems[0:n_elements(time)-1,*]
endif

if keyword_set(noq) then begin
  q_peri = elems[*,0]
  q_ap = elems[*,0]
endif else begin
  q_peri = elems[*,0]*( 1 - elems[*,1] )
  q_ap = elems[*,0]*( 1 + elems[*,1] )
endelse

if keyword_set(noq) then ys=1 else ys=9
plot_nosci, time, [min(q_peri),max(q_ap)], /nodata, /xs, ys=ys, $
            xma=[10,10], yma=[0,0], xtickn=replicate(' ',20), $
            ytit='Semimajor Axis'+aunits, tit=tit
if not keyword_set(noq) then begin
  axis, yaxis=1, yr=!y.crange, /ys, ytit='Periapse and Apoapse, '+cn
endif
oplot, time, elems[*,0], ps=ps
if keyword_set(interv) then oplot, time[interv], elems[interv,0], ps=ps, co=ctred()
if not keyword_set(noq) then begin
  oplot, time, q_peri, ps=ps, l=l, co=clr
  oplot, time, q_ap, ps=ps, l=l, co=clr
endif

plot_nosci, time, elems[*,1], /xs, ys=9, xma=[10,10], yma=[0,0], ps=ps, $
            xtickn=replicate(' ',20), ytit='Eccentricity'
axis, yaxis=1, yr=[min(elems[*,2]),max(elems[*,2])], /ys, /save, $
      ytit='Inclination'+angunits+', '+cn
oplot, time, elems[*,2], ps=ps, l=l, co=clr

plot, time, [0,angmax], /nodata, /xs, ys=9, xma=[10,10], yma=[0,0], ps=ps, $
      xtit=xtit, ytit='Longitude of Node'+angunits, yticki=angmax/4
axis, yaxis=1, yr=!y.crange, /ys, ytit='Argument of Periapse'+angunits+', '+cn
wrapap = wrapflag(elems[*,4])
for j=0,n_elements(wrapap)-2 do begin
  oplot, time[wrapap[j]:wrapap[j+1]-1], elems[wrapap[j]:wrapap[j+1]-1,4], $
         ps=ps, l=l, co=clr
endfor
if keyword_set(trendap) then begin
  xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.02, chars=1.5, $
          !y.crange[0] + (!y.crange[1]-!y.crange[0])*.05, trendap, co=clr
endif
wrapln = wrapflag(elems[*,3])
for j=0,n_elements(wrapln)-2 do begin
  oplot, time[wrapln[j]:wrapln[j+1]-1], elems[wrapln[j]:wrapln[j+1]-1,3], $
         ps=ps
endfor
if keyword_set(trendln) then begin
  xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.02, chars=1.5, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])*.1, trendln
endif

!y.omargin = yoma
!p.multi = pm

end
