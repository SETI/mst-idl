function elems_to_cart, _elems, vel=vel, gm=gm, musat=musat, musun=musun, au=au, rsat=rsat, day=day, year=year, yr2pi=yr2pi, debug=debug

if n_params() eq 0 then begin
  print, 'Syntax:  pos = elems_to_cart( elems, vel=vel, gm=gm )
  print, 'All angles are in RADIANS.
  print, 'Input elems is an nx6 vector with the form '
  print, '[ [a], [e], [i], [long_node], [arg_peri], [m] ].'
  return, -1
endif

; For input of the 6 orbital elements, returns the position and velocity
; in Cartesian coordinates.
; Outputs pos and vel are nx3 vectors.
; Input elems has dimensions nx6. Columns are different bodies, and rows are: 
; 0: a (in AU)
; 1: e
; 2: i
; 3: long_node
; 4: arg_peri
; 5: mean_anom
; All angles are in radians

; Reform elems to dimensions that facilitate creating the trans matrix.
elems = _elems
if (size(elems))[2] ne 6 then stop, 'elems must be nx6.'
n = (size(elems))[1]
elems = reform( transpose( elems, [1,0] ), 6, 1, n )

; Make sure all angles are in the range from 0 to 2 pi.
elems[2:5,*,*] = fix_angles( elems[2:5,*,*], /rad, /to360 )

;; See Murray and Dermott, p.35
;if (where(elems[1,*,*] gt 0.6627434))[0] ne -1 then stop, $
;	'Series solution to Kepler Equation diverges for e < 0.6627434'

; Eccentric anomaly, from numerical solution to Kepler's Equation.
ecc_anom = ( elems[5,*,*] + elems[1,*,*]*sin( $
	elems[5,*,*] + elems[1,*,*]*sin( $
	elems[5,*,*] + elems[1,*,*]*sin( $
	elems[5,*,*] + elems[1,*,*]*sin( $
	elems[5,*,*] + elems[1,*,*]*sin( $
	elems[5,*,*] + elems[1,*,*]*sin( $
        elems[5,*,*]) ) ) ) ) ) )
; True anomaly
true_anom = acos( (cos(ecc_anom)-elems[1,*,*]) / $
	(1-elems[1,*,*]*cos(ecc_anom)) )
; The range of acos() is 0 to !pi.  If the eccentric anomaly is greater 
; than !pi, then the true anomaly is as well.
foo = where( ecc_anom gt !dpi, count )
if count gt 0 then true_anom[foo] = 2*!dpi - true_anom[foo]

; Create transformation matrix from orbital-plane coords to 3-D coords.
; From Murray and Dermott, Eqs.2.119 and 2.120, rotate about z-axis by angle
; arg_peri, about x-axis by angle i, about z-axis by angle long_node:
tm = [ [ cos(elems[4,*,*])*cos(elems[3,*,*])-$
                sin(elems[4,*,*])*cos(elems[2,*,*])*sin(elems[3,*,*]), $
         -sin(elems[4,*,*])*cos(elems[3,*,*])-$
                cos(elems[4,*,*])*cos(elems[2,*,*])*sin(elems[3,*,*]), $
         sin(elems[2,*,*])*sin(elems[3,*,*]) ], $
       [ cos(elems[4,*,*])*sin(elems[3,*,*])+$
                sin(elems[4,*,*])*cos(elems[2,*,*])*cos(elems[3,*,*]), $
         -sin(elems[4,*,*])*sin(elems[3,*,*])+$
                cos(elems[4,*,*])*cos(elems[2,*,*])*cos(elems[3,*,*]), $
         -sin(elems[2,*,*])*cos(elems[3,*,*]) ], $
       [ sin(elems[4,*,*])*sin(elems[2,*,*]), $
         cos(elems[4,*,*])*sin(elems[2,*,*]), $
         cos(elems[2,*,*]) ] ]

; Get Cartesian coords in orbital plane.
pos = polar_to_cart([ [replicate(0d,n)], [reform(true_anom)], $
	[reform(elems[0,*,*] * ( 1 - elems[1,*,*]*cos(ecc_anom) ))] ])
; Transform to 3-D coords.
pos = v_matrix_multiply( tm, pos )

; Get Cartesian velocities in orbital plane.
; Can only get velocities if the value of mu has been given.
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
if keyword_set(au) then gm = gm / 1.4959787d8^3
if keyword_set(rsat) then gm = gm / 60330.0d0^3
if keyword_set(day) then gm = gm * (86400.0d0)^2
if keyword_set(year) then gm = gm * (365.25*86400.0d0)^2
if keyword_set(yr2pi) then gm = gm * (365.25*86400.0d0/2/!dpi)^2
if keyword_set(gm) then begin
  vel = rebin(reform( sqrt(gm/elems[0,*,*]/(1-elems[1,*,*]^2)) ),n,3)
  vel = vel * [ [reform( -sin(true_anom) )], $
             [reform( elems[1,*,*] + cos(true_anom) )], [replicate(0d,n)] ]
  ; Transform to 3-D coords.
  vel = v_matrix_multiply( tm, vel )
endif
if keyword_set(debug) then stop

return, pos

end
