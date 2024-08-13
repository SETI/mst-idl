restore, 'lc82_mkg.sav'

rres_lc82_mst = fltarr(nn)
torque_lc82_mst = fltarr(nn)
x_nl_lc82_mst = fltarr(nn)
x_max_lc82_mst = fltarr(nn)
rres_mst = fltarr(nn)
torque_mst = fltarr(nn)
x_nl_mst = fltarr(nn)
x_max_mst = fltarr(nn)
for j=0,nn-1 do begin
  torque_lc82_mst[j] = restorque( ll[j], mm[j], moon[j], /lc82, $
                                  rres=_rres, x_nl=_x_nl, x_max=_x_max )
  rres_lc82_mst[j] = _rres / 1e5
  x_nl_lc82_mst[j] = _x_nl
  x_max_lc82_mst[j] = _x_max
  torque_mst[j] = restorque( ll[j], mm[j], moon[j], $
                                  rres=_rres, x_nl=_x_nl, x_max=_x_max ) * 1e20
  rres_mst[j] = _rres
  x_nl_mst[j] = _x_nl
  x_max_mst[j] = _x_max
endfor

moons = moon[uniq(moon)]

!y.margin = 0
!y.omargin = [4,2]
!p.multi = [0,1,4]
!p.charsize = 1.5
notn = replicate(' ',20)
if not keyword_set(mode) then mode = 0
_tit = 'Ratio with LC82 Published Tables!CUsing LC82 Values'
for j=0,3 do begin
  case j of
    0: begin
      x1 = rres_lc82_mst
      x2 = rres_mst
      x3 = rres
      ytit = 'R_res'; (km)'
    end 
    1: begin
      x1 = torque_lc82_mst
      x2 = torque_mst
      x3 = torque
      ytit = 'Torque'
      ;if mode eq 1 then ytit = ytit + '!C(fractional change)'
    end 
    2: begin
      x1 = x_nl_lc82_mst
      x2 = x_nl_mst
      x3 = x_nl
      ytit = 'X_NL'
      ;if mode eq 1 then ytit = ytit + '!C(fractional change)'
    end 
    3: begin
      x1 = x_max_lc82_mst
      x2 = x_max_mst
      x3 = x_max
      ytit = 'X_max'
      ;if mode eq 1 then ytit = ytit + '!C(fractional change)'
    end 
  endcase
  if mode eq 1 then begin
    ;p1 = x1 - x3
    ;if j ne 0 then p1 = ( x1 - x3 )/x3
    p1 = x1/x3
    ylog = 0
  endif else if mode eq 2 then begin
    p1 = abs(x3)
    if j eq 0 then ylog=0 else ylog = 1
    _tit = 'Modern Values'
  endif else begin
    ;p1 = x2 - x3
    ;p2 = x1 - x3
    ;if j ne 0 then p1 = ( x2 - x3 )/x3
    ;if j ne 0 then p2 = ( x1 - x3 )/x3
    p1 = x2/x3
    p2 = x1/x3
    ylog = 0
    _tit = _tit + ' (dashed) and Modern Values (solid)'
  endelse 
  if j eq 0 then tit=_tit else tit=''
  ;if j eq 3 then xtn='' else xtn=notn
  xtn = notn
  plot, p1, ytit=ytit, /xs, /ynozero, xtickn=xtn, tit=tit, ylog=ylog, xtickle=1e-10
  if mode eq 0 then oplot, p2, l=5
  for k=0,n_elements(moons)-1 do begin
    oplot, [1,1]*min(where(moon eq moons[k])), !y.crange, l=1
    if k eq 1 then cr='!C' else cr=''
    if j eq 3 then xyouts, mean(where(moon eq moons[k])), $
                           !y.crange[0] - (!y.crange[1]-!y.crange[0])/10, $
                           cr+naifsat(moons[k]), charsize=1, align=.5
  endfor 
endfor

end
