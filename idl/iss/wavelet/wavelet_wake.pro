if keyword_set(wake) then begin
  for jj=0,nwakes-1 do oplot, r, wakepredict[*,jj], l=2
;  oplot, r, wakepredict[*,0]*2, l=1
;  oplot, r, wakepredict[*,0]*3, l=1
endif 
;stop
xxmax=0
if not keyword_set(xxmax) then xxmax = nt
phase = get_phase( wave )
if keyword_set(atlas54) then _wave = wave[*,0:48] else _wave = wave
if imn eq 10 then _wave = wave[*,0:58]
totphase = get_phase( total(_wave,2), wrap=totwrap )
zeros = where( sign(totphase[1:nt-1]) ne sign(totphase[0:nt-2]) and $
               abs(totphase[1:nt-1]) lt 90 ) + 1
if keyword_set(ph) then begin
  ph = ph mod 360
  foo = where( ph gt 180, count )
  if count gt 0 then ph[foo] = ph[foo] - 360
  phwrap = get_phase_wrap(ph)
  phzeros = where( sign(ph[1:nt-1]) ne sign(ph[0:nt-2]) and $
               abs(ph[1:nt-1]) lt 90 ) + 1
endif
if imn eq 34 then begin
  xx0 = 300
  xx1 = 1000
endif else begin
  xx0 = 0
  xx1 = nt
endelse
int = lindgen(xx1-xx0) + xx0
if !d.name eq 'X' then window, 2, xs=1024, ys=512
!p.multi=[0,1,2]
plot, r[int], _v[int], /xs, /ys, ytit='I/F', xtickn=replicate(' ',20), yma=[1,2]
oplot, r[zeros], _v[zeros], ps=4, co=red()
if keyword_set(ph) then oplot, r[phzeros], _v[phzeros], ps=7, co=red()
plot,r[int],totphase[int],ps=4, /xs, /ys, yr=[-180,180], yticki=60, /nodata, $
     xtit=xtit, ytit='Phase', yma=[4,-1]
for kk=0,n_elements(totwrap)-2 do oplot, r[totwrap[kk]:totwrap[kk+1]-1], $
     totphase[totwrap[kk]:totwrap[kk+1]-1]
if keyword_set(ph) then for kk=0,n_elements(phwrap)-2 do oplot, $
     r[phwrap[kk]:phwrap[kk+1]-1], ph[phwrap[kk]:phwrap[kk+1]-1], l=1

if keyword_set(wake) then begin
  if !d.name eq 'X' then window, 1, xs=1024, ys=1024
  wakephase = fltarr( xxmax, nwakes )
  for jj=0,nwakes-1 do wakephase[*,jj] = interpolate( phase, lindgen(xxmax), $
                   interpol(lindgen(j+1),period,wakepredict[0:xxmax-1,jj]) )
  wakephaserate = fltarr( xxmax-1, nwakes )
  for jj=0,nwakes-1 do wakephaserate[*,jj] = wakephase[1:xxmax-1,jj] - $
                   wakephase[0:xxmax-2,jj]
  foo = where( wakephaserate gt 180, count )
  if count gt 0 then wakephaserate[foo] = wakephaserate[foo] - 360
  foo = where( wakephaserate lt -180, count )
  if count gt 0 then wakephaserate[foo] = wakephaserate[foo] + 360

  if 4 eq 5 then begin
    !p.multi = 0
    dr = r[1]-r[0]
    plot, r[[0,xxmax-1]], [0,100]/dr, /nodata, xtit=xtit, ytit='Phase Change Rate (deg/km)'
    clr = [ red(), blue(), green(), yellow() ]
    for jj=0,nwakes-1 do oplot, r, wakephaserate[*,jj]/dr, ps=4, co=clr[jj]
    for jj=1,nwakes do oplot, r, wakephaserate[*,0]/dr*jj, ps=3, co=yellow()
    for jj=1,nwakes do oplot, r, wakephaserate[*,0]/dr+(jj-1)*90, ps=3, co=cyan()
    phaserate = fltarr( nt-1, j+1 )
    for jj=0,j do phaserate[*,jj] = phase[1:nt-1,jj] - phase[0:nt-2,jj]
    foo = where( phaserate gt 60 )
    phaserate[foo] = phaserate[foo] - 180
  endif

  if 4 eq 5 then begin
    window, 2
    plot, yr, [0,100]/dr, /nodata, xtit=ytit, ytit='Phase Change Rate (deg/km)'
    for jj=0,nwakes-1 do oplot, wakepredict[*,jj], wakephaserate[*,jj]/dr, ps=-4, co=clr[jj]
    oplot, period, rebin(phaserate,1,j+1)/dr
    oplot, period, period*180/!pi, co=yellow()
    ; As you can see, the phase rate is simply related to wavenumber, as it should be.
  endif

  xx0 = 50 & xx1 = 450
  clr = [ red(), blue(), green(), yellow() ]
  int = lindgen(xx1-xx0) + xx0
  if !d.name eq 'X' then window, 3
  !p.multi=0
  plot, [0,120], [min(r[int]),max(r[int])], /nodata
  for jj=nwakes-1,0,-1 do begin
    zeros = where( abs(wakephase[1:xxmax-1,jj]) lt 90 and $
        sign(wakephase[1:xxmax-1,jj]) ne sign(wakephase[0:xxmax-2,jj]) ) + 1
    oplot, indgen(n_elements(zeros))*2.^(2-jj), r[zeros], ps=-4, co=clr[jj]
  endfor
  wset, 1
  solid_diamonds
  !p.multi=[0,1,4]
  plot, r[int], _v[int], /xs, /ys, ytit='I/F', xtickn=replicate(' ',20), $
        yma=[-14,2]
  for jj=nwakes-1,0,-1 do begin
    zeros = where( abs(wakephase[1:xxmax-1,jj]) lt 90 and $
        sign(wakephase[1:xxmax-1,jj]) ne sign(wakephase[0:xxmax-2,jj]) ) + 1
    oplot, r[zeros], _v[zeros], ps=8, co=clr[jj]
  endfor
  for jj=0,nwakes-1 do begin
    if jj eq nwakes-1 then begin
      _xt = xtit 
      xtn = replicate(' ',20)
    endif else begin
      _xt = ''
      xtn = ''
    endelse
    plot, r[int], wakephase[int,jj], /xs, /ys, yr=[-180,180], yticki=60, $
       /nodata, xtit=_xt, ytit='Phase', xtickn=xtn, yma=[-8+6*jj,14-6*jj]
    wrap = get_phase_wrap( wakephase[*,jj] )
    for kk=0,n_elements(wrap)-2 do oplot, r[wrap[kk]:wrap[kk+1]-1], $
       wakephase[wrap[kk]:wrap[kk+1]-1,jj], co=clr[jj]
  endfor

endif

end
