;cspice_furnsh, getenv("CAVIAR_KERNELS")
cspice_furnsh, '/home/sauron2/iss/NAIF/spk/de721_full.bsp'

sun = 10L
saturn = 6L;699L
aimpoint_coord = [[0d0],[0],[0]]
polera = 40.58756d0
poledec = 83.53684d0
vertical = polar_to_cart([ [poledec*!dpi/180], [polera*!dpi/180], [1] ])

if keyword_set(check1995) then begin
  ; Presentation by Mark Showalter shows the equinox beginning
  ; at 1995 NOV 17.76 and ending at 1995 NOV 21.50
  ; http://saturn.jpl.nasa.gov/files/20090929_CHARM_Showalter.pdf
  timestart = caldate( '1995 NOV 10', /reverse )
  timeend = caldate( '1995 NOV 28', /reverse )
  dt = 0.1d
  eqxsurvey = 0
endif
if not keyword_exists(eqxsurvey) then eqxsurvey = 1
if keyword_set(eqxsurvey) then begin
  ;timestart = caldate( '1899 JUL 29', /reverse )
  ;timeend = caldate( '2053 OCT 08', /reverse )
  timestart = caldate( '1608 JAN 01', /reverse )
  timeend = caldate( '2056 JAN 01', /reverse )
  dt = 10d
endif
nt = (timeend-timestart)/dt + 1
time = dindgen(nt)*dt + timestart
epoch = 2451545.0d0

incidence = dblarr(nt)
for j=0l,nt-1 do begin
  _time = (time[j]-epoch)*86400
  cspice_spkez, sun, _time, 'J2000', 'NONE', saturn, state, ltime
  sun_coord = rotate(state[0:2],1)
  incidence[j] = (incidence_angle( aimpoint_coord, sun_coord, $
                                   vertical ))[0]*180/!dpi
endfor

year = (time-epoch)/365.25 + 2000
eqx95yr = 1995.8823d0
eqx95jd = (eqx95yr-2000)*365.25 + epoch
;eqxpast = floor((eqx95yr-year[0])/14.61)
;eqxfuture = floor((year[nt-1]-eqx95yr)/14.61)
eqxpast = floor((eqx95jd-time[0])/14.61/365.25)
eqxfuture = floor((time[nt-1]-eqx95jd)/14.61/365.25)
nex = eqxpast + eqxfuture + 1
;eqxstart = eqx95yr + 14.61*(indgen(nex)-eqxpast)
eqxstart = eqx95jd + 14.61*365.25*(indgen(nex)-eqxpast)
;eqxyear = dblarr(nex)
eqxjd = dblarr(nex)
for j=0,nex-1 do begin
  ;foo = where( year gt eqxstart[j]-1 and year lt eqxstart[j]+1 )
  foo = where( time gt eqxstart[j]-365 and time lt eqxstart[j]+365 )
  ;eqxyear[j] = interpol( year[foo], incidence[foo], 90 )
  eqxjd[j] = interpol( time[foo], incidence[foo], 90 )
endfor
eqxyear = (eqxjd-epoch)/365.25 + 2000
solid_circles
if not keyword_set(noplot) then begin
  plot, year, incidence, /xs, /ys, xtit='Year', ytit='Incidence Angle (!Uo!N)'
  oplot, eqxyear, replicate(90,nex), ps=8
endif

print, ''
print, 'Saturn Equinox Dates:'
;print, caldate((eqxyear-2000)*365.25+epoch)
print, caldate(eqxjd)

moons = [ 'Titan', 'Iapetus', 'Rhea', 'Tethys', 'Dione', 'Mimas', 'Enceladus', $
          'Hyperion', 'Phoebe', 'Janus', 'Helene', 'Calypso', 'Telesto' ]
; Moon discovery dates from http://planetarynames.wr.usgs.gov/Page/Planets
moondisc = [ '1655 MAR 25', '1671 OCT 25', '1672 DEC 23', '1684 MAR 21', $
             '1684 MAR 21', '1789 JUL 18', '1789 AUG 28', '1848 SEP 16', $
             '1898 AUG 16', '1966 DEC 15', '1980 MAR 01', '1980 MAR 13', $
             '1980 APR 08' ]
moondiscjd = caldate( moondisc, /reverse )

print, ''
print, 'Moon         Discovery       Equinox       Diff'
print, '----         ---------       -------       ----'
for k=0,n_elements(moons)-1 do begin
  nearesteqx = (where( abs(moondiscjd[k]-eqxjd) eq $
                       min(abs(moondiscjd[k]-eqxjd)) ))[0]
  fo = '("' + strjoin([moons[k],replicate(' ',12-strlen(moons[k]))]) + '","' + $
       moondisc[k] + '","    ","' + strmid(caldate(eqxjd[nearesteqx]),0,11) + $
       '","    ",I5)'
  print, fo=fo, moondiscjd[k] - eqxjd[nearesteqx]
endfor
print, ''

end
