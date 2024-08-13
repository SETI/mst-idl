function cart_to_elems1, _pos, _vel, gm=gm, musat=musat, musun=musun, deg=deg, true_anom=true_anom, rsat=rsat, au=au, day=day, year=year, yr2pi=yr2pi

if n_params() eq 0 then begin
  print, 'Syntax:  elems = cart_to_elems1( pos, vel, gm=gm, deg=deg )'
  print, 'Inputs pos and vel are nx3 vectors.'
  return, -1
endif

; For input of Cartesian position and velocity, returns the 6 orbital elements.
; Inputs pos and vel are nx3 vectors.
; Output elems has dimensions nx6. Columns are different bodies, and rows are: 
; 0: a (in AU)
; 1: e
; 2: i
; 3: long_node
; 4: arg_peri
; 5: mean_anom
; All angles are in radians

; Mu must be specified
if keyword_set(musun) then begin
  cap_g = 6.6721629d-20 ;km^3/kg/s^2
  msun = 1.9891219d30   ;kg
  gm = cap_g * msun
  print, 'Using gm = '+strtrim(gm,2)+' km^3/s^2, the value for the Sun.'
endif
if keyword_set(musat) then begin
  saturn_constants, gm=gm
  ;gm = 37931289.4836  ; from cpck05May2004.tpc
  print, 'Using gm = '+strtrim(gm,2)+' km^3/s^2, the value for Saturn.'
endif
; If units are AU-(yr/2pi), then GM = 1
; If units are AU-day, then GM = au^3/sday^2/cap_g/msun = 0.00029592339
; If units are KM-S, then GM = GM_sun = 1.3271745e11
if not keyword_set(gm) then begin
  print, 'Gm must be specified in units of km and seconds.'
  print, 'Use /musun for the Sun, or /musat for Saturn'
  stop
endif
pos = _pos
vel = _vel
n = n_elements(pos[*,0])

if keyword_set(au) then begin
  prad = 1.4959787d8
  pos = pos * prad
  vel = vel * prad
endif
if keyword_set(rsat) then begin
  saturn_constants, prad=prad
  pos = pos * prad
  vel = vel * prad
endif
if keyword_set(day) then begin
  vel = vel / 86400.0d0
endif
if keyword_set(year) then begin
  vel = vel / 86400.0d0 / 365.25
endif
if keyword_set(yr2pi) then begin
  timeu = 365.25d0 * 86400 / 2 / !dpi
  vel = vel / timeu
endif

; Vector magnitudes r and v.
r = v_mag(pos)
v = v_mag(vel)

; Angular momentum vector
h = v_cross( pos, vel )

; Rate of change in vector r.
rdot = sign(v_inner( pos, vel )) * sqrt( v^2 - v_mag(h)^2/r^2 )

; Semimajor axis
a = ( 2/r - v^2/gm )^(-1)

; Laplace-Runge-Lenz vector
lrlv = rebin(( v^2 - gm/r ),n,3)*pos - rebin(v_inner( pos, vel ),n,3)*vel

; Eccentricity
e = v_mag(lrlv) / gm

; Inclination
i = acos( h[*,2] / v_mag(h) )

; Longitude of ascending node
sin_long_node = sign(h[*,2]) * h[*,0] / v_mag(h) / sin(i)
cos_long_node = -sign(h[*,2]) * h[*,1] / v_mag(h) / sin(i)
long_node = get_angle( sin_long_node, cos_long_node, /rad, /to360 )

; Longitude of pericenter
long_peri = atan( lrlv[*,1] / lrlv[*,0] )
; The range of atan() is -!pi/2 to !pi/2.  If lrlv[*,0] lt 0 then left quadrant
foo = where( lrlv[*,0] lt 0, count )
if count gt 0 then long_peri[foo] = long_peri[foo] + !dpi

; Argument of pericenter
arg_peri = fix_angles( long_peri - long_node, /rad, /to360 )

; True anomaly
true_anom = (cart_to_polar(pos))[*,1] - long_peri

; Eccentric anomaly
ecc_anom = acos( ( 1 - r/a )/e )
; The range of acos() is 0 to !pi.  If the true anomaly is greater than !pi, 
; then the eccentric anomaly is as well.
foo = where( true_anom gt !dpi, count )
if count gt 0 then ecc_anom[foo] = 2*!dpi - ecc_anom[foo]

; Mean anomaly, from Kepler's Equation
m = ecc_anom - e * sin(ecc_anom)

elems = [ [a], [e], [i], [long_node], [arg_peri], [m] ]
if keyword_set(deg) then elems[*,2:5] = elems[*,2:5] * 180 / !dpi
if keyword_set(rsat) then elems[*,0] = elems[*,0] / prad

return, elems

end
