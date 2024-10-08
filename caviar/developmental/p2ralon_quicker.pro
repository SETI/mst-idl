
pro p2ralon_quicker,cmat,et,polera,poledec,sc,RA,dec,radius,lon,$
                    outofplane=outofplane, light_time=light_time, $
                    xyz_rp=xyz_rp, n_iter=n_iter

capN=(polera+90.0d0)*2.0d0*!dpi/360.0
capJ=(90.0d0-poledec)*2.0d0*!dpi/360.0

rot=dblarr(3,3)

rot[0,0]=cos(capN)
rot[1,0]=-sin(capN)*cos(capJ)
rot[2,0]=sin(capN)*sin(capJ)
rot[0,1]=sin(capN)
rot[1,1]=cos(capN)*cos(capJ)
rot[2,1]=-cos(capN)*sin(capJ)
rot[0,2]=0.0
rot[1,2]=sin(capJ)
rot[2,2]=cos(capJ)

szx = size(RA) 
szy = size(dec) 
if (where(szx ne szy))[0] ne -1 then stop, szx, szy 
nn = szx[szx[0]+2] 
if nn gt 1 then begin 
  RA = reform(RA,nn) 
  dec = reform(dec,nn) 
endif 

rho_J2000=dblarr(nn,3)
xyz_J2000=dblarr(nn,3)

r=1.0d0*cos(2.0d0*!dpi*dec/360.0d0)
rho_J2000[*,0]=r*cos(2.0d0*!dpi*RA/360.0d0)
rho_J2000[*,1]=r*sin(2.0d0*!dpi*RA/360.0d0)
rho_J2000[*,2]=(sin(2.0d0*!dpi*dec/360.0d0))/1.0d0

trot=transpose(rot)

rho=trot##rho_J2000

;light_time=0.0d0
light_time = reform(dblarr(nn))
done = intarr(nn)

outofplane = 0

if not keyword_exists(n_iter) then n_iter = 1
;for i=0,19 do begin
;for i=0,0 do begin
for i=0,n_iter do begin
  ; Note that setting the aberration correction argument (abcorr)
  ; to 'LT' corrects for light travel time on the fly.  'CN' uses
  ; a "converged Newtonian" which we probably don't need.  Adding
  ; '+S' causes it to do stellar aberration as well.  See spk.req
  ; Change 14 Oct 2005: Use CN+S, since manual abcorr never fully implemented.
  ; Change 28 Jan 2008: Use NONE again and also iterate i=0,1 instead of i=0,0
  ;    now conforming to Mike Evans' version of p2ralon in Caviar 0.65
  if i eq 0 and n_elements(et) eq 1 then begin
    ;cspice_spkez,699L,et-mean(light_time),'J2000','CN+S',sc,state,ltime
    cspice_spkez,699L,et-mean(light_time),'J2000','NONE',sc,state,ltime
    xyz_J2000 = rebin( reform(-state[0:2],1,3), nn, 3 )
  endif else for j=0l,nn-1 do if done[j] ne 1 then begin
    ;cspice_spkez,699L,et-light_time[j],'J2000','CN+S',sc,state,ltime
    if n_elements(et) eq 1 then begin
      cspice_spkez,699L,et-light_time[j],'J2000','NONE',sc,state,ltime
    endif else begin
      cspice_spkez,699L,et[j]-light_time[j],'J2000','NONE',sc,state,ltime
    endelse
    xyz_J2000[j,*]=-state[0:2]
  endif

  xyz=trot##xyz_J2000

  foo = where( xyz[*,2] lt 0.0 and rho[*,2] le 0.0, count )
  if count gt 0 then begin
    ;stop
    print, 'Pixel points away from the ring plane.  Cannot define ring radius and longitude.'
    outofplane = 1
    radius=0.0d0
    lon=0.0d0
    return
  endif

  foo = where( xyz[*,2] gt 0.0 and rho[*,2] ge 0.0, count )
  if count gt 0 then begin
    ;stop
    print, 'Pixel points away from the ring plane.  Cannot define ring radius and longitude.'
    outofplane = 1
    radius=0.0d0
    lon=0.0d0
    return
  endif

  length=abs(xyz[*,2]/rho[*,2])

  _light_time = light_time
  light_time = (length*v_mag(rho))/299792.458d0
  foo = where( light_time eq _light_time, donecount )
  if donecount gt 0 then done[foo] = 1
  if donecount eq nn then goto, skip

endfor

skip:
xyz_rp = xyz + ( rebin(length,nn,3) * rho )

radius=v_mag(xyz_rp)

lon=(360.0d0*atan(xyz_rp[*,1],xyz_rp[*,0]))/(2.0d0*!dpi)

foo = where( lon lt 0.0, count )
if count gt 0 then lon[foo] = lon[foo] + 360.0d0

if nn gt 1 then if szx[0] eq 2 then begin 
  radius = reform( radius, szx[1], szx[2] )
  lon = reform( lon, szx[1], szx[2] ) 
  RA = reform( RA, szx[1], szx[2] ) 
  dec = reform( dec, szx[1], szx[2] ) 
endif 

end

