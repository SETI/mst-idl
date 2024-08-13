function caviar_omega2, r, domega_dr, domega2_dr=domega2_dr, lc82=lc82, gm=_gm, prad=_prad, j2=_j2, j4=_j4, j6=_j6, j8=_j8, noj8=noj8, var=var

if n_params() eq 0 then begin
  print, 'Syntax:  Result = CAVIAR_OMEGA2( r, domega_dr, var= )'
  print, 'Returns the square of the mean motion (radians/sec) for radius r in Saturn''s ring plane,'
  print, 'accounting for Saturn''s j2, j4, j6, and j8.  '
  print, 'Set var=''kappa'' or var=''nu'' to calculate variables other than omega.'
  print, 'Optionally returns the derivative (radians/sec/km) as well.'
  retall
endif

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
  j8 = 0d
endif
; Overwrite with input values, if they exist
if keyword_exists(_gm) then gm = _gm
if keyword_exists(_prad) then prad = _prad
if keyword_exists(_j2) then j2 = _j2
if keyword_exists(_j4) then j4 = _j4
if keyword_exists(_j6) then j6 = _j6
if keyword_exists(_j8) then j8 = _j8

; Legendre polynomials and their derivatives (will be needed in the following)
; P_2(x) = (3/2)x^2 - 1/2 
;      ==> P_2(0) = -1/2 and P''_2(0) = 3 and P''''_2(0) = 0
; P_4(x) = (35/8)x^4 - (15/4)x^2 + 3/8 
;      ==> P_4(0) = 3/8 and P''_4(0) = -15/2 and P''''_2(0) = 105
; P_6(x) = (231/16)x^6 - (315/16)x^4 + (105/16)x^2 - 5/16
;      ==> P_6(0) = -5/16 and P''_6(0) = 105/8 and P''''_2(0) = -945/2
; P_8(x) = (6435/128)x^8 - (3003/32)x^6 + (3465/64)x^4 -(315/32)x^2 + 35/128
;      ==> P_8(0) = 35/128 and P''_8(0) = -315/16 and P''''_2(0) = 10395/8
; For derivatives in the following, note that each term is proportional to 
; r^{-i-3}, and that the first term (not in the Sum_i) is for i=0.  
if not keyword_set(var) then var='omega'
case var of
  'omega': begin
    ; From MD99 Eq 6.241, omega^2 = (1/a) ( partial V / partial r )_0
    ; where MD99 Eq 6.218 gives V = -gm/r * ( 1 - Sum_i P_i(0)*J_i*(prad/r)^i ) 
    ; omega^2 = gm/r^3 * ( 1 - Sum_i (i+1)*P_i(0)*J_i*(prad/r)^i )
    ; As derived above (just above the beginning of the case statement):
    ; P_2(0) = -1/2; P_4(0) = 3/8; P_6(0) = -5/16; P_8(0) = 35/128
    omega2 = gm / r^3 * ( 1 + j2*3/2*(prad/r)^2 - j4*15/8*(prad/r)^4 $
                            + j6*35/16*(prad/r)^6 - j8*315/128*(prad/r)^8 )
    ; d(omega^2)/dr = gm/r^4 * ( -3 + Sum_i (i+3)*(i+1)*P_i(0)*J_i*(prad/r)^i )
    domega2_dr = gm / r^4 * ( -3 - j2*15/2*(prad/r)^2 + j4*105/8*(prad/r)^4 $
                                 - j6*315/16*(prad/r)^6 $
                                 + j8*3465/128*(prad/r)^8 )
    ; d(omega^2)/dr = 2*omega*d(omega)/dr
    domega_dr = domega2_dr / 2 / sqrt(omega2)
  end 
  'kappa': begin
    ; From MD99 Eq 6.242, kappa^2 = (3/a) ( partial V / partial r )_0 
    ;                               + ( partial^2 V / partial r^2 )_0
    ; where MD99 Eq 6.218 gives V = -gm/r * ( 1 - Sum_i P_i(0)*J_i*(prad/r)^i ) 
    ; ==> dV/dr = gm/r^2 * ( 1 - Sum_i (i+1)*P_i(0)*J_i*(prad/r)^i )
    ; ==> d2V/dr2 = -gm/r^3 * ( 2 - Sum_i (i+1)*(i+2)*P_i(0)*J_i*(prad/r)^i )
    ; kappa^2 = gm/r^3 * ( 1 + Sum_i (i+1)*(i-1)*P_i(0)*J_i*(prad/r)^i )
    ; As derived above (just above the beginning of the case statement):
    ; P_2(0) = -1/2; P_4(0) = 3/8; P_6(0) = -5/16; P_8(0) = 35/128
    kappa2 = gm / r^3 * ( 1 - j2*3/2*(prad/r)^2 + j4*45/8*(prad/r)^4 $
                            - j6*175/16*(prad/r)^6 + j8*2205/128*(prad/r)^8 )
    ; d(kappa^2)/dr = gm/r^4 * ( -3 
    ;                        - Sum_i (i+3)*(i+1)*(i-1)*P_i(0)*J_i*(prad/r)^i )
    dkappa2_dr = gm / r^4 * ( -3 + j2*15/2*(prad/r)^2 - j4*315/8*(prad/r)^4 $
                                 + j6*1575/16*(prad/r)^6 $
                                 - j8*24255/128*(prad/r)^8 )
    ; d(kappa^2)/dr = 2*kappa*d(kappa)/dr
    dkappa_dr = dkappa2_dr / 2 / sqrt(kappa2)
    ; Enter into output variables
    omega2 = kappa2
    domega2_dr = dkappa2_dr
    domega_dr = dkappa_dr
  end
  'nu': begin
    ; From MD99 Eq 6.243, nu^2 = (1/a) ( partial V / partial r )_0
    ;                            + (1/a^2) (partial^2 V / partial alpha^2 )_0
    ; where MD99 Eq 6.218 gives 
    ; V = -gm/r * ( 1 - Sum_i P_i(sin(alpha))*J_i*(prad/r)^i ) 
    ; ==> dV/dr = gm/r^2 * ( 1 - Sum_i (i+1)*P_i(0)*J_i*(prad/r)^i )
    ; Let sin(alpha)=x.  The Legendre polynomials are evaluated at x=0.
    ; Also, d[P_i(sin(alpha))]/d(alpha) = d[P_i(x)]/dx*cos(alpha) = d[P_i(x)]/dx
    ; ==> dV/d(alpha) = Sum_i (gm/r)*J_i*(prad/r)^i*P'_i(0)
    ; ==> d2V/d(alpha)2 = Sum_i (gm/r)*J_i*(prad/r)^i*P''_i(0)
    ; nu^2 = gm/r^3 * ( 1 - Sum_i J_i*(prad/r)^i*[ (i+1)*P_i(0) - P''_i(0) ] )
    ; As derived above (just above the beginning of the case statement):
    ; P_2(0) = -1/2; P_4(0) = 3/8; P_6(0) = -5/16; P_8(0) = 35/128
    ; P''_2(0) = 3; P''_4(0) = -15/2; P''_6(0) = 105/8; P''_8(0) = -315/16
    nu2 = gm / r^3 * ( 1 + j2*9/2*(prad/r)^2 - j4*75/8*(prad/r)^4 $
                         + j6*245/16*(prad/r)^6 - j8*2835/128*(prad/r)^8 )
    ; d(nu^2)/dr = gm/r^4 * ( -3 + Sum_i (i+3)*J_i*(prad/r)^i
    ;                                       *[ (i+1)*P_i(0) - P''_i(0) ] )
    dnu2_dr = gm / r^4 * ( -3 - j2*45/2*(prad/r)^2 + j4*525/8*(prad/r)^4 $
                              - j6*2205/16*(prad/r)^6 $
                              + j8*31185/128*(prad/r)^8 )
    ; d(nu^2)/dr = 2*nu*d(nu)/dr
    dnu_dr = dnu2_dr / 2 / sqrt(nu2)
    ; Enter into output variables
    omega2 = nu2
    domega2_dr = dnu2_dr
    domega_dr = dnu_dr
  end
  'eta': begin
    ; Borderies-Rappaport and Longaretti (1994, Icarus) Eq A4 gives
    ; eta^2 = gm/r^3 * ( 1 + Sum_i (i+1)*(i-1)*(i/2+3)/3*P_i(0)*J_i*(prad/r)^i )
    ; As derived above (just above the beginning of the case statement):
    ; P_2(0) = -1/2; P_4(0) = 3/8; P_6(0) = -5/16; P_8(0) = 35/128
    eta2 = gm / r^3 * ( 1 - j2*2*(prad/r)^2 + j4*75/8*(prad/r)^4 $
                          - j6*175/8*(prad/r)^6 + j8*5145/128*(prad/r)^8 )
    ; d(eta^2)/dr = gm/r^4 * ( -3 
    ;                - Sum_i (i+3)*(i+1)*(i-1)*(i/2+3)/3*P_i(0)*J_i*(prad/r)^i )
    deta2_dr = gm / r^4 * ( -3 + j2*10*(prad/r)^2 - j4*525/8*(prad/r)^4 $
                               + j6*1575/8*(prad/r)^6 $
                               - j8*56595/128*(prad/r)^8 )
    ; d(eta^2)/dr = 2*eta*d(eta)/dr
    deta_dr = deta2_dr / 2 / sqrt(eta2)
    ; Enter into output variables
    omega2 = eta2
    domega2_dr = deta2_dr
    domega_dr = deta_dr
  end
  'chi': begin
    ; Borderies-Rappaport and Longaretti (1994, Icarus) Eq A5 gives
    ; chi^2 = gm/r^3 * ( 1 - Sum_i (i+3)/3*J_i*(prad/r)^i
    ;                                *[ (i+1)*P_i(0) - P''_i(0) ] )
    ; As derived above (just above the beginning of the case statement):
    ; P_2(0) = -1/2; P_4(0) = 3/8; P_6(0) = -5/16; P_8(0) = 35/128
    ; P''_2(0) = 3; P''_4(0) = -15/2; P''_6(0) = 105/8; P''_8(0) = -315/16
    chi2 = gm / r^3 * ( 1 + j2*15/2*(prad/r)^2 - j4*175/8*(prad/r)^4 $
                          + j6*735/16*(prad/r)^6 - j8*10395/128*(prad/r)^8 )
    ; d(chi^2)/dr = gm/r^4 * ( -3 - Sum_i (i+3)*(i+3)/3*J_i*(prad/r)^i
    ;                                       *[ (i+1)*P_i(0) - P''_i(0) ] )
    dchi2_dr = gm / r^4 * ( -3 - j2*75/2*(prad/r)^2 + j4*1225/8*(prad/r)^4 $
                            - j6*6615/16*(prad/r)^6 + j8*114345/128*(prad/r)^8 )
    ; d(chi^2)/dr = 2*chi*d(chi)/dr
    dchi_dr = dchi2_dr / 2 / sqrt(chi2)
    ; Enter into output variables
    omega2 = chi2
    domega2_dr = dchi2_dr
    domega_dr = dchi_dr
  end 
  'beta': begin
    ; Borderies-Rappaport and Longaretti (1994, Icarus) Eq A6 gives
    ; beta^2 = gm/r^3 * ( 1 - Sum_i (i+4)*(i+3)*(i+2)*(i+1)/24
    ;                                 *P_i(0)*J_i*(prad/r)^i )
    ; As derived above (just above the beginning of the case statement):
    ; P_2(0) = -1/2; P_4(0) = 3/8; P_6(0) = -5/16; P_8(0) = 35/128
    beta2 = gm / r^3 * ( 1 + j2*15/2*(prad/r)^2 - j4*105/4*(prad/r)^4 $
                           + j6*525/8*(prad/r)^6 - j8*17325/128*(prad/r)^8 )
    ; d(beta^2)/dr = gm/r^4 * ( -3 - Sum_i (i+3)*(i+4)*(i+3)*(i+2)*(i+1)/24
    ;                                 *P_i(0)*J_i*(prad/r)^i )
    dbeta2_dr = gm / r^4 * ( -3 - j2*75/2*(prad/r)^2 + j4*735/4*(prad/r)^4 $
                             - j6*4725/8*(prad/r)^6 + j8*190575/128*(prad/r)^8 )
    ; d(beta^2)/dr = 2*beta*d(beta)/dr
    dbeta_dr = dbeta2_dr / 2 / sqrt(beta2)
    ; Enter into output variables
    omega2 = beta2
    domega2_dr = dbeta2_dr
    domega_dr = dbeta_dr
  end
  'delta': begin
    ; Borderies-Rappaport and Longaretti (1994, Icarus) Eq A7 gives
    ; delta^2 = gm/r^3 * ( 1 - Sum_i (i+3)*(i+4)/12*J_i*(prad/r)^i
    ;                                  *[ (i+1)*P_i(0) - P''_i(0) ] )
    ; As derived above (just above the beginning of the case statement):
    ; P_2(0) = -1/2; P_4(0) = 3/8; P_6(0) = -5/16; P_8(0) = 35/128
    ; P''_2(0) = 3; P''_4(0) = -15/2; P''_6(0) = 105/8; P''_8(0) = -315/16
    delta2 = gm / r^3 * ( 1 + j2*45/4*(prad/r)^2 - j4*175/4*(prad/r)^4 $
                            + j6*3675/32*(prad/r)^6 - j8*31185/128*(prad/r)^8 )
    ; d(delta^2)/dr = gm/r^4 * ( -3 - Sum_i (i+3)*(i+3)*(i+4)/12*J_i*(prad/r)^i
    ;                                         *[ (i+1)*P_i(0) - P''_i(0) ] )
    ddelta2_dr = gm / r^4 * ( -3 - j2*225/4*(prad/r)^2 + j4*1225/4*(prad/r)^4 $
                                 - j6*33075/32*(prad/r)^6 $
                                 + j8*343035/128*(prad/r)^8 )
    ; d(delta^2)/dr = 2*delta*d(delta)/dr
    ddelta_dr = ddelta2_dr / 2 / sqrt(delta2)
    ; Enter into output variables
    omega2 = delta2
    domega2_dr = ddelta2_dr
    domega_dr = ddelta_dr
  end 
  'lambda': begin
    ; Borderies-Rappaport and Longaretti (1994, Icarus) Eq A9 gives
    ; lambda^2 = gm/r^3 * ( 1 - Sum_i J_i*(prad/r)^i*[ (i+1)*(i+3)/3*P_i(0) 
    ;                                                  - 2*(i+3)/3*P''_i(0)
    ;                                                  + 1/9*P''''_i(0) ] )
    ; As derived above (just above the beginning of the case statement):
    ; P_2(0) = -1/2; P_4(0) = 3/8; P_6(0) = -5/16; P_8(0) = 35/128
    ; P''_2(0) = 3; P''_4(0) = -15/2; P''_6(0) = 105/8; P''_8(0) = -315/16
    ; P''''_2(0) = 0; P''''_4(0) = 105; 
    ; P''''_6(0) = -945/2; P''''_8(0) = 10395/8
    lambda2 = gm / r^3 * ( 1 + j2*25/2*(prad/r)^2 - j4*1225/24*(prad/r)^4 $
                             + j6*2205/16*(prad/r)^6 - j8*38115/128*(prad/r)^8 )
    ; d(lambda^2)/dr = gm/r^4 * ( 1 - Sum_i (i+3)*J_i*(prad/r)^i
    ;                                         *[ (i+1)*(i+3)/3*P_i(0) 
    ;                                             - 2*(i+3)/3*P''_i(0)
    ;                                             + 1/9*P''''_i(0) ] )
    dlambda2_dr = gm / r^4 * ( -3 - j2*125/2*(prad/r)^2 $
                                  + j4*8575/24*(prad/r)^4 $
                                  - j6*19845/16*(prad/r)^6 $
                                  + j8*419265/128*(prad/r)^8 )
    ; d(lambda^2)/dr = 2*lambda*d(lambda)/dr
    dlambda_dr = dlambda2_dr / 2 / sqrt(lambda2)
    ; Enter into output variables
    omega2 = lambda2
    domega2_dr = dlambda2_dr
    domega_dr = dlambda_dr
  end 
  else: begin
    stop, 'var unrecognized'
  end
endcase

return, omega2

end
