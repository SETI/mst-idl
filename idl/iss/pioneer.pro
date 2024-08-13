; pioneer.pro
; Query the trajectory kernel for Pioneer 11.  
; Find the closest approach distance to Saturn's rings

cspice_furnsh, '/home/borogove/iss/NAIF/spk/p11-a.bsp'

; et=0 at the J2000 epoch, a.k.a. 2000 JAN 01 12:00:00.000
epoch = julday(1,1,2000,12,0,0)   ; 2451545.0d0

ok = [ 0, 3, 5, 6, 10, -24 ]
okl = [ 'Solar System Barycenter', 'Earth-Moon Barycenter', $
        'Jupiter Barycenter', 'Saturn Barycenter', 'Sun', 'Pioneer 11' ]
if keyword_set(_sat) then sat = _sat else sat = -24L
if keyword_set(_obs) then obs = _obs else obs = 6L
;tbase = julday( month, day, year, hour, min, sec )
if keyword_set(_tbase) then tbase = double(_tbase) else tbase = epoch - 7426.92
if keyword_set(_nt) then nt = _nt else nt = 1001
if keyword_set(_dt) then dt = double(_dt) else dt = 0.000218d  ; in days
tmin = -9766.39878               ;-3488.5
tminl = '1973 APR 06 02:25:45'   ;'1990 JUN 14 00:00:00'
tmax = -3561.5                   ;5475.5
tmaxl = '1990 JAN 01 00:00:00'   ;'2014 DEC 29 00:00:00'
t0 = -7427
t0l = '1979 SEP 01'
if not keyword_set(gring) then gring = 0
if keyword_set(cassini) then begin
  tmin = 431
  tminl = '2001 MAR 07 12 00 00'
  tmax = 3142.5
  tmaxl = '2008 AUG 09 00:00:00'
  sat = -82L
  ok = [ ok, -82 ]
  okl = [ okl, 'Cassini' ]
  cspice_furnsh, getenv("CAVIAR_KERNELS")
  tbase = epoch + 1642.5
  t0 = 1643
  t0l = '2004 JUL 01'
  psname = 'cassinitraj'
endif else if gring eq 1 then begin
  tbase = epoch - 7426.905
  nt = 101
  psname = 'pioneerg1'
endif else if gring eq 2 then begin
  tbase = epoch - 7426.74
  nt = 101
  psname = 'pioneerg2'
endif else begin
  psname = 'pioneertraj'
endelse
if (where( sat eq ok ))[0] eq -1 then begin
  print, 'Satellite is restricted to the following:  '
  print, rotate([[string(ok,fo='(I3)')],[okl]],4)
  help, sat
  print, 'Continue?'
  stop
endif
if obs ge 11 and obs ne 699 then begin
  print, 'Observer is restricted to the following: '
  print, rotate([[string(ok,fo='(I3)')],[okl]],4)
  help, obs
  print, 'Continue?'
  stop
endif

if tbase-epoch lt tmin then begin
  print, ''
  print, 'Requested start time is at JD '+strtrim(tbase,2)+', which is '+caldate(tbase)
  print, 'Kernel contains information only after '+tminl
  print, 'Resetting start time to '+strtrim(epoch+tmin,2)
  tbase = epoch + tmin
endif
if tbase+dt*(nt-1)-epoch gt tmax then begin
  print, ''
  print, 'Requested end time is at JD '+strtrim(tbase+dt*(nt-1)-epoch,2)+', which is '+caldate(tbase+dt*(nt-1)-epoch)
  print, 'Kernel contains information only before '+tmaxl
  nt = long( (tmax+epoch-tbase) / dt + 1 )
  print, 'Resetting start time to '+strtrim(epoch+tmax,2)+' (nt to '+strtrim(nt,2)+')'
endif
print, ''
time = tbase + dt*lindgen(nt) - epoch  ; in days from J2000
print, 'Using dt = '+strtrim(dt,2)+' days, and nt = '+strtrim(nt,2)+$
       ', so duration is '+strtrim(dt*(nt-1),2)+' days.'
print, 'Time begins at JD '+strtrim(tbase,2)+', which is '+caldate(tbase)
print, 'Time ends at JD '+strtrim(tbase+dt*(nt-1),2)+', which is '+caldate(tbase+dt*(nt-1))
print, ''

;foo = where( time lt tmin, count )
;if count gt 0 then begin
;  print, 'Kernel contains information only after '+tminl
;  print, 'You are asking for times earlier than that.  Continue?'
;  stop
;endif
;foo = where( time gt tmax, count )
;if count gt 0 then begin
;  print, 'Kernel contains information only before '+tmaxl
;  print, 'You are asking for times later than that.  Continue?'
;  stop
;endif

pos = dblarr(nt,3)
vel = dblarr(nt,3)
if not keyword_set(ref) then ref = 'J2000'
for j=0l,nt-1 do begin
  _time = time[j] * 86400
  cspice_spkez,sat,_time,ref,'NONE',obs,state,ltime
  pos[j,*] = state[0:2]
  vel[j,*] = state[3:5]
  ;caldate, time[j], month, day, year, hour, min, sec
endfor

if ref eq 'J2000' then begin
  ; Saturn pole RA and Dec, values from Caviar (pck00007.tpc)
  polera=40.58756d0
  poledec=83.53684d0
  ; Convert position and velocity from J2000 to Saturn coordinate system.
  pos = j2000_to_saturn( pos, polera, poledec )
  vel = j2000_to_saturn( vel, polera, poledec )
endif
; Convert Cartesian position and velocity into orbital elements 
; ( a, e, i, long_node, arg_peri, mean_anom )
elems = cart_to_elems( pos, vel, /musat, /deg )
; Convert to polar coordinates
pos_polar = cart_to_polar( pos )
cpring_polar = pos_polar
cpring_polar[*,0] = 0  ; Set z=0
foo = where( cpring_polar[*,2] gt 136780, count )
if count gt 0 then cpring_polar[foo,2] = 136780
cpring = polar_to_cart( cpring_polar )
ringdist = v_mag( pos - cpring )

if not keyword_exists(noplot) then noplot=1
if not keyword_set(noplot) then begin
  if keyword_set(dolzr) then begin
    lzr, psname, /half
    @plot_prepare
  endif
  ;plot_elems, elems, time=time, /tkm, ps=3
  !y.omargin = [4,2]
  !y.margin = 0
  !p.multi = [0,1,3]
  !p.charsize = 2;1.5
  xtn = replicate(' ',20)
  plot, (time-t0)*24, tkm(v_mag(pos)), /xs, /ys, xtickn=xtn, $
        ytit='Saturn Distance!C'+tkmtit()
  oplot, !x.crange, 136.78*[1,1], l=1
  if keyword_set(gring) then begin
    oplot, !x.crange, 167*[1,1], l=1
    oplot, !x.crange, 173*[1,1], l=1
  endif
  plot, (time-t0)*24, tkm(pos[*,2]), /xs, /ys, xtickn=xtn, $
        ytit='Height Above Ringplane!C'+tkmtit()
  oplot, !x.crange, [0,0], l=1
  if keyword_set(gring) then begin
    plot, (time-t0)*24+12, pos_polar[*,1]*360/!dpi, /xs, /ys, $
          ytit='Longitude (!Uo!N)', xtit='Time on '+t0l+' (hr)'
  endif else begin
    plot, (time-t0)*24+12, tkm(ringdist), /xs, /ys, $
          ytit='Distance from Rings!C'+tkmtit(), xtit='Time on '+t0l+' (hr)'
  endelse
  if keyword_set(dolzr) then clzr
endif

end
