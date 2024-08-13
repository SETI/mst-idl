; Calculate phase functions using Mie calculations originally from
; Mark Showalter, as discussed by Hedman et al (2009, AJ). 
nph = 183
q1 = 2;3;1
q2 = 5
dq = 1
nqq = (q2-q1+1)/dq
scat = fltarr(nph)
p1 = fltarr(nph,nqq)
p2 = fltarr(nph,nqq)
p3 = fltarr(nph,nqq)
for qq=q1,q2 do begin
  openr, 1, '/home/sauron2/vims/Phase/PL-Mie/pl'+strtrim(qq,2)+'0_0500.pf'
  aa = ''
  readf, 1, aa
  while strmid(aa,0,6) ne 'CosBar' do readf, 1, aa
  for j=0,nph-1 do begin
    readf, 1, aa
    bb = strsplit( aa, /extract )
    if n_elements(bb) ne 4 then stop
    scat[j] = bb[0]
    p1[j,qq-q1] = bb[1]
    p2[j,qq-q1] = bb[2]
    p3[j,qq-q1] = bb[3]
  endfor
  if not eof(1) then stop
  if qq eq q1 then _scat = scat else begin
    if (where( _scat ne scat ))[0] ne -1 then stop
  endelse 
  close, 1
endfor
phase = 180-scat

if keyword_set(dolzr) then begin
  lzr, 'mie_phase_functions'
  @plot_prepare
endif
!p.multi = [0,2,2]
if q1 eq 3 then ymin = 4e-2 else ymin = 1e-2
plot, [0], [0], xr=[180,60], yr=[ymin,1e2], /nodata, /xs, /ys, /ylog, $
      xticki=30, xtit='Phase Angle (!Uo!N)', ytit='Phase Function P(!Ma)'
for qq=q1,q2 do oplot, phase, p3[*,qq-q1]
for qq=q1,q2 do xyouts, 80, p3[where( phase eq 80 ),qq-q1]*1.1, $
                        'q='+strtrim(qq,2)
arrowlength = [ 3., 3, 1./3, 1./3 ]
textspacex = [ 1, -1, 1.5, 1.5 ]
textspacey = [ .9, .9, 1.15, 1.15 ]
align = [ 1, 1, 0, 0 ]
obstext = [ 'C1-C6 (non-equinox)', 'Bx and Cx!C(equinox)', 'Ax(1)', 'Ax(2)' ]
xx = [ 174, 150, 90.8, 100.3 ]
yy = [ 1, .5, 5, 5 ]
nphase = n_elements(xx)
for j=0,nphase-1 do begin
  arrow, xx[j], yy[j], xx[j], yy[j]*arrowlength[j], hsize=!d.x_size/128, $
         hthick=1, /solid, thick=!p.thick, /data
  xyouts, xx[j]-textspacex[j], yy[j]*textspacey[j], orient=90, align=align[j], $
          obstext[j]
endfor
solid_circles
for qq=q1,q2 do oplot, xx, interpol( p3[*,qq-q1], phase, xx ), ps=8
if keyword_set(dolzr) then clzr

stop
qindex = indgen(q2-q1+1) + q1
phaseindex = xx
if not keyword_set(qbase) then qbase = 1
phasefac = fltarr(nphase,nqq)
for qq=q1,q2 do phasefac[*,qq-q1] = interpol( p3[*,qq-q1], phase, phaseindex )
print, 'q    C1--C6      B, C        A1        A2'
if keyword_set(tex) then begin
  for qq=q1,q2 do print, qq, phasefac[*,qbase] / phasefac[*,qq-q1], fo='(I1'+strjoin(replicate('," & ",F10.3',nphase))+'," \\")'
endif else begin
  for qq=q1,q2 do print, qq, phasefac[*,qbase] / phasefac[*,qq-q1], fo='(I1'+strjoin(replicate(',F10.3',nphase))+')'
endelse

stop, 'Save?'
save, qindex, phaseindex, phasefac, filename='mie_phase_functions.sav'

end
