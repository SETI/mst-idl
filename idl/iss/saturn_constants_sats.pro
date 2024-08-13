  pro saturn_constants_sats, gm=gm, prad=prad, j2=j2, j4=j4, j6=j6, lc82=lc82

;  if n_params() eq 0 then begin
;    print, 'Syntax:  SATURN_CONSTANTS, gm, prad, j2, j4, j6'
;    print, 'Fills in the currently used values for Saturn''s mass, radius, '+$
;           'and gravity harmonics.'
;    retall
;  endif

  ; Saturn mass, radius, and harmonics from JPL kernel cpck05May2004.tpc
  gm = 37931289.4836 ;km^3/s^2
  prad = 60330.0d0 ;km
  j2 = 0.016292243237d
  j4 = -0.000928716077d
  j6 = 0.000088845313

  if keyword_set(lc82) then begin
    ; The values used by Lissauer and Cuzzi 1982
    gm = 37929141.6d ;km^3/s^2
    prad = 60330.0d0 ; km
    j2 = 16299.1d-6
    j4 = -916.7d-6
    j6 = 81.3d-6
  endif

  end
