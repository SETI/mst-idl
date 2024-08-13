if not keyword_exists(paperplot) then paperplot = 1
if keyword_set(paperplot) then version = 1
if not keyword_set(version) then version = 1
if keyword_set(old) then begin
  rimpact = 130000.0d0
  ; These were the hours from initial impact in the simple keplerian case
  hoursbetween = 49.15 - 23.15   
  yr1 = 90-[80.5,90]
  yr2 = [-1.1,1.1]
endif else begin
;  rimpact = 129382.0d0
  rimpact = 129350.0d0
  hoursbetween = 88168.0d0 / 3600
  yr1 = [0,9.5]
  yr2 = [-2,2]
endelse
;rimpact = 125000.0d0
Omega = sqrt(caviar_omega2(rimpact))  ; in radians/s
Omegat = dindgen(601)/100*2*!dpi
kappa = sqrt(caviar_kappa2(rimpact))  ; in radians/s
kappat = dindgen(601)/100*2*!dpi / Omega * kappa
;Omega = kappa
if keyword_set(old) then begin
  ; Equations 24 and 25 of Schmidt white paper, version 21 Nov 2010
  tanphi1 = 2*( 5*sin(Omegat) - 3*Omegat )
  tanphi2 = sqrt(2*( 73 + 18*Omegat^2 - 64*cos(Omegat) - $
                     9*cos(2*Omegat) - 60*Omegat*sin(Omegat) ))
  tanphi = (tanphi1-tanphi2)/( 16*(sin(Omegat/2))^2 )
  tanphi = -1/tanphi  ; Convert to my preferred convention
endif else begin
  tanphi = ( 1 - cos(Omegat) )/( 1.5*Omegat - 2*sin(Omegat) )
;  tanphi = ( 1 - cos(Omegat+!dpi/4) )/( 1.5*(Omegat+!dpi/4) - 2*sqrt(2)*sin(Omegat+!dpi/4) )
  ;tanphi = ( 1 - cos(kappat) )/( 1.5*Omegat - 2*sin(kappat) )
endelse
device, decomposed=0
!p.multi = [0,2,2]
!y.margin = 0
!y.omargin = [4,2]
if version eq 2 or version eq 3 or version eq 5 then begin
  xtit = 'Orbits'
  xtn = ''
endif else begin
  xtit = ''
  xtn = replicate(' ',20)
endelse
plot, [min(Omegat),max(Omegat)]/2/!dpi, [0,0], yr=yr1, /nodata, /xs, /ys, $
      xtit=xtit, xtickn=xtn, ytit='Cant Angle (!Uo!N)'
polyfill, !x.crange[[0,1,1,0,0]], 3.46+[-1,-1,1,1,-1]*.02, co=gray()
polyfill, !x.crange[[0,1,1,0,0]], 1.63+[-1,-1,1,1,-1]*.04, co=gray()
oplot, Omegat/2/!dpi, fix_angles( atan(tanphi)*180/!dpi, /deg, /to360 )
if keyword_set(tiscareno) then begin
  oplot, co=green(), Omegat/2/!dpi, fix_angles( atan(( 1 - cos(Omegat) )/$
         ( 1.5*Omegat - 2*sin(Omegat) ))*180/!dpi, /deg, /to360 )
  oplot, co=blue(), Omegat/2/!dpi, fix_angles( (( 1 - cos(Omegat) )/$
         ( 1.5*Omegat - 2*sin(Omegat) ))*180/!dpi, /deg, /to360 )
  oplot, co=yellow(), Omegat/2/!dpi, 1/1.5d0/Omegat*180/!dpi
endif
oplot, Omegat/2/!dpi, 90-atan(1.5*Omegat)*180/!dpi, l=5
if version eq 5 then stop
bnd = [0,1,1.5,2,2.5,3,3.47,4]
for j=0,6 do begin
  if j eq 0 and version eq 1 then begin
    solid_circles
    l=0
;  endif else if j eq 2 then begin
;    open_circles
;    l=2
  endif else begin
    open_circles;solid_tiny_circles
    l=1
  endelse 
  foo = where( Omegat/2/!dpi gt bnd[j] and Omegat/2/!dpi lt bnd[j+1] )
  _root = interpol( Omegat[foo]/2/!dpi, fix_angles( atan(tanphi[foo])*180/!dpi,$
                                                    /deg, /to360 ), 3.46 )
  if j eq 0 then root = _root else root = [ root, _root ]
  if version eq 2 then begin
    oplot, [_root], [3.46], ps=-8, co=red()
  endif else begin
    oplot, _root+[0,hoursbetween*Omega*3600/2/!dpi], [3.46,1.63], $
           ps=-8, l=l, co=red()
  endelse
endfor
if keyword_set(paperplot) then begin
  xyouts, !x.crange[1] - (!x.crange[1] - !x.crange[0])/15, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/15, chars=1, '(a)'
  xyouts, !x.crange[1] - (!x.crange[1] - !x.crange[0])/15, 3.48, chars=1, $
          'Observation A1', align=1
  xyouts, !x.crange[0] + (!x.crange[1] - !x.crange[0])/15, 1.67, chars=1, $
          'Observation A2'
endif
if version eq 2 or version eq 3 then stop

!p.multi[0] = !p.multi[0]-1
if !d.name eq 'X' then yma=[20,0] else yma=[13,0]
if not keyword_set(old) then yma = yma + [-1,1]/2.
plot, [0,6], yr2, /nodata, /xs, /ys, yma=yma, $
      xtit='Orbits', ytit='Second Detection!CCant Angle Residual (!Uo!N)'
polyfill, !x.crange[[0,1,1,0,0]], [-1,-1,1,1,-1]*sqrt(.04^2+.02^2), co=gray()
for j=0,6 do begin
  tt = root[j]+hoursbetween*Omega*3600/2/!dpi
  _residual = 1.63 - interpol( fix_angles( atan(tanphi)*180/!dpi, $
                                           /deg, /to360 ), Omegat/2/!dpi, tt )
  if j eq 0 then residual = _residual else residual = [ residual, _residual ]
  if j eq 0 and version eq 1 then solid_circles else open_circles
  oplot, [tt], [_residual], ps=8, co=red()
endfor
if keyword_set(paperplot) then begin
  xyouts, !x.crange[1] - (!x.crange[1] - !x.crange[0])/15, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/7, chars=1, '(b)'
endif

end
