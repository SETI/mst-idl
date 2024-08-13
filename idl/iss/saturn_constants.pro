  pro saturn_constants, gm=gm, prad=prad, j2=j2, j4=j4, j6=j6, j8=j8, lc82=lc82, may04=may04, whizin11=whizin11

  ;if n_params() eq 0 then begin
  ;  print, 'Syntax:  SATURN_CONSTANTS, gm, prad, j2, j4, j6'
  ;  print, 'Fills in the currently used values for Saturn''s mass, radius, '+$
  ;         'and gravity harmonics.'
  ;  retall
  ;endif

  ; Saturn mass, radius, and harmonics from Jacobson et al (2006)
  gm = 37931207.7d0    ;km^3/s^2
  prad = 60330.0d0   ;km
  j2 = 16290.71d-6
  j4 = -935.83d-6
  j6 = 86.14d-6
  j8 = 10.0d-6

  if keyword_set(whizin11) then begin
    ; Parameters from file supplied by Akbar Whizin on 7 September 2011
    gm = 3.794058506225147E+07
    prad = 60330.0d0
    j2 = 1.629049740703108E-02
    j4 = -9.366596484062808E-04
    j6 = 8.569863144704968E-05
    j8 = -1.000000000000000E-05
  endif 

  if keyword_set(may04) then begin
    ; Saturn mass, radius, and harmonics from JPL kernel cpck05May2004.tpc
    gm = 37931289.4836 ;km^3/s^2
    prad = 60330.0d0   ;km
    j2 = 0.016292243237d
    j4 = -0.000928716077d
    j6 = 0.000088845313
    j8 = 0
  endif

  if keyword_set(lc82) then begin
    ; The values used by Lissauer and Cuzzi 1982
    gm = 37929141.6d ;km^3/s^2
    prad = 60330.0d0 ; km
    j2 = 16299.1d-6
    j4 = -916.7d-6
    j6 = 81.3d-6
    j8 = 0
  endif

  end
