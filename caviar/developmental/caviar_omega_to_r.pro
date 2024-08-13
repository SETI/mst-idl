function rsat_root, r
  common omega_to_r1, lc82, omega, gm, prad, j2, j4, j6, j8, noj8, var
  return, caviar_omega2(r,lc82=lc82,j2=j2,j4=j4,j6=j6,j8=j8,noj8=noj8,gm=gm,prad=prad,var=var) - omega^2
end

function caviar_omega_to_r, omega, lc82=lc82, gm=_gm, prad=_prad, j2=_j2, j4=_j4, j6=_j6, j8=_j8, noj8=noj8, var=var

if n_params() eq 0 then begin
  print, 'Syntax:  Result = CAVIAR_OMEGA_TO_R( omega )'
  print, 'For input mean motion (radians/sec) returns geometric orbital radius r in Saturn''s ring plane,'
  print, 'accounting for Saturn''s j2, j4, and j6.  '
  retall
endif

common omega_to_r1, _lc82, _omega, gm, prad, j2, j4, j6, j8, _noj8, _var
if keyword_set(lc82) then _lc82 = lc82
if keyword_set(noj8) then _noj8 = noj8
if keyword_set(var) then _var = var

;; Saturn mass, radius, and harmonics from JPL kernel cpck05May2004.tpc
;gm = 37931289.4836 ;km^3/s^2
;prad = 60330.0d0 ;km
;j2 = 0.016292243237d
;j4 = -0.000928716077d
;j6 = 0.000088845313
saturn_constants, gm=gm, prad=prad, j2=j2, j4=j4, j6=j6, j8=j8
if not keyword_set(j8) then j8=0
if keyword_set(noj8) then j8=0
if keyword_set(lc82) then begin
  ; Use values from Lissauer and Cuzzi (1982)
  gm = 37929141.6d ;km^3/s^2
  j2 = 16299.1d-6
  j4 = -916.7d-6
  j6 = 81.3d-6
endif
; Overwrite with input values, if they exist
if keyword_exists(_j2) then j2 = _j2
if keyword_exists(_j4) then j4 = _j4
if keyword_exists(_j6) then j6 = _j6
if keyword_exists(_j8) then j8 = _j8

nom = n_elements(omega)
rsat = dblarr(nom)
for j=0,nom-1 do begin
  _omega = omega[j]
  ; Start with provisional satellite orbital radius (fx_root wants a 3-element
  ; vector for reasons I don't understand)
  rsat0 = ( gm / omega[j]^2 ) ^ (1./3) + [-5,0,5]
  ; Find precise satellite orbital radius 
  rsat[j] = fx_root( rsat0, 'rsat_root', /double )
endfor
if nom eq 1 then rsat = rsat[0]

return, rsat

end
