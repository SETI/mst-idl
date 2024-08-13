; This script assumes that the image time (et for ephemeris time), the location
; of Saturn's pole (polera and poledec), and the appropriate Spice kernels have 
; already been loaded, perhaps by Caviar.  ( If not, then @get_sat_prepare ).

; The &$ at the end of each line is to make the script compatible with
; being embedded within another script as part of a loop. 

cspice_spkez, sat, et, 'J2000', 'NONE', 699l, state, ltime &$
sat_xyz = j2000_to_saturn( reform(state[0:2],1,3), polera, poledec ) &$
sat_polar = cart_to_polar(sat_xyz) &$
sat_polar[0:1] = sat_polar[0:1] * 180 / !dpi &$

;end
