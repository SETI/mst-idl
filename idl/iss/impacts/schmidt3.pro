;if not keyword_exists(paperplot) then paperplot = 1
if not keyword_set(version) then version = 1
if keyword_set(paperplot) and version ne 6 then version = 1
if keyword_set(maintext) then version = 6
if keyword_set(maintext) then paperplot = 1
if not keyword_set(old) then old = 0
if old eq 2 then begin
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
  if keyword_set(maintext) then yr1 = [0,5]
endelse
;rimpact = 129250.0d0
;rimpact = 129500.0d0
;rimpact = 125000.0d0
Omega = sqrt(caviar_omega2(rimpact))  ; in radians/s
Omegat = dindgen(601)/100*2*!dpi
kappa = sqrt(caviar_kappa2(rimpact))  ; in radians/s
kappat = dindgen(601)/100*2*!dpi / Omega * kappa
;Omega = kappa
if old eq 2 then begin
  ; Equations 24 and 25 of Schmidt white paper, version 21 Nov 2010
  tanphi1 = 2*( 5*sin(Omegat) - 3*Omegat )
  tanphi2 = sqrt(2*( 73 + 18*Omegat^2 - 64*cos(Omegat) - $
                     9*cos(2*Omegat) - 60*Omegat*sin(Omegat) ))
  tanphi = (tanphi1-tanphi2)/( 16*(sin(Omegat/2))^2 )
  tanphi = -1/tanphi  ; Convert to my preferred convention
endif else if keyword_set(phi0) then begin
  tanphi = ( -.5*sin(phi0)*sin(Omegat) + cos(phi0)*(1-cos(Omegat)) )/$
           ( -sin(phi0)*(1-cos(Omegat)) + cos(phi0)*(1.5*Omegat-2*sin(Omegat)) )
endif else begin
  tanphi = ( 1 - cos(Omegat) )/( 1.5*Omegat - 2*sin(Omegat) )
;  tanphi = ( 1 - cos(Omegat+!dpi/4) )/( 1.5*(Omegat+!dpi/4) - 2*sqrt(2)*sin(Omegat+!dpi/4) )
  ;tanphi = ( 1 - cos(kappat) )/( 1.5*Omegat - 2*sin(kappat) )
endelse
phi = fix_angles( atan(tanphi)*180/!dpi, /deg )
foo = (where( phi - shift(phi,1) gt 150 ))[0]
phi[0:foo-1] = phi[0:foo-1] + 180
if version eq 6 then phi = 90-atan(1.5*Omegat)*180/!dpi
device, decomposed=0
!p.multi = [0,2,2]
!y.margin = 0
!y.omargin = [4,2]
if version eq 2 or version eq 3 or version eq 5 or $
   keyword_set(maintext) then begin
  xtit = 'Orbits'
  xtn = ''
endif else begin
  xtit = ''
  xtn = replicate(' ',20)
endelse
plot, [min(Omegat),max(Omegat)]/2/!dpi, [0,0], yr=yr1, /nodata, /xs, /ys, $
      xtit=xtit, xtickn=xtn, ytit='Cant Angle !Mq (!Uo!N)'
if keyword_set(old) then begin
  a1cant = 3.46
  a1cant_sigma = .02
  a2cant = 1.63
  a2cant_sigma = .04
endif else begin
  a1cant = 3.3956
  a1cant_sigma = .01
  a2cant = 1.6803
  a2cant_sigma = .0136
endelse
polyfill, !x.crange[[0,1,1,0,0]], a1cant+[-1,-1,1,1,-1]*a1cant_sigma, co=gray()
polyfill, !x.crange[[0,1,1,0,0]], a2cant+[-1,-1,1,1,-1]*a2cant_sigma, co=gray()
oplot, Omegat/2/!dpi, phi
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
if version eq 6 then bnd = [0,4]
for j=0,n_elements(bnd)-2 do begin
  if ( j eq 5 and version eq 1 ) or version eq 6 then begin
    solid_circles
    l=0
;  endif else if j eq 2 then begin
;    open_circles
;    l=2
  endif else begin
    open_circles;solid_tiny_circles
    l=1
  endelse 
  if version eq 6 then solid_circles
  foo = where( Omegat/2/!dpi gt bnd[j] and Omegat/2/!dpi lt bnd[j+1] )
  _root = interpol( Omegat[foo]/2/!dpi, phi[foo], a1cant )
  if j eq 0 then root = _root else root = [ root, _root ]
  if version eq 2 then begin
    oplot, [_root], [a1cant], ps=-8, co=red()
  endif else begin
    oplot, _root+[0,hoursbetween*Omega*3600/2/!dpi], [a1cant,a2cant], $
           ps=-8, l=l, co=red()
  endelse
  if keyword_set(maintext) then for k=-1,1,2 do begin
    open_circles
    oplot, _root+[0,hoursbetween*Omega*3600/2/!dpi]+k*1, [a1cant,a2cant], $
           ps=-8, l=1, co=red()
    arrow, mean(_root+[0,hoursbetween*Omega*3600/2/!dpi])+k*0.2, $
           mean([a1cant,a2cant]), $
           mean(_root+[0,hoursbetween*Omega*3600/2/!dpi])+k*0.8, $
           mean([a1cant,a2cant]), $
           hthick=1, hsize=!d.x_size/128, thick=3, /solid, /data, color=red()
  endfor
endfor
if keyword_set(paperplot) then begin
  xyouts, !x.crange[1] - (!x.crange[1] - !x.crange[0])/30, $
          a1cant+a1cant_sigma, 'Observation Ax(1)', chars=1, align=1
  xyouts, !x.crange[0] + (!x.crange[1] - !x.crange[0])/30, $
          a2cant+a2cant_sigma, 'Observation Ax(2)', chars=1
  if keyword_set(maintext) then begin
    xyouts, !x.crange[0] + (!x.crange[1] - !x.crange[0])/30, $
            !y.crange[1] - (!y.crange[1]-!y.crange[0])/17.5, chars=1, '(c)'
    stop
  endif 
  xyouts, !x.crange[1] - (!x.crange[1] - !x.crange[0])/15, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/15, chars=1, '(a)'
endif
if version eq 2 or version eq 3 then stop

!p.multi[0] = !p.multi[0]-1
if !d.name eq 'X' then yma=[20,0] else yma=[13,0]
if old ne 2 then yma = yma + [-1,1]/2.
plot, [0,6], yr2, /nodata, /xs, /ys, yma=yma, $
      xtit='Orbits', ytit='Second Detection!CCant Angle Residual (!Uo!N)'
polyfill, !x.crange[[0,1,1,0,0]], $
          [-1,-1,1,1,-1]*sqrt(a2cant_sigma^2+a1cant_sigma^2), co=gray()
for j=0,n_elements(root)-1 do begin
  tt = root[j]+hoursbetween*Omega*3600/2/!dpi
  _residual = a2cant - interpol( phi, Omegat/2/!dpi, tt )
  if j eq 0 then residual = _residual else residual = [ residual, _residual ]
  if j eq 5 and version eq 1 then solid_circles else open_circles
  if version eq 6 then solid_circles
  oplot, [tt], [_residual], ps=8, co=red()
endfor
if keyword_set(paperplot) then begin
  xyouts, !x.crange[1] - (!x.crange[1] - !x.crange[0])/15, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/7, chars=1, '(b)'
endif

end
