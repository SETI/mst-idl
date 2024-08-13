function fundamental_freq, aa, ee, ii, gm=_gm, prad=_prad, j2=_j2, j4=_j4, j6=_j6, j8=_j8, noj8=noj8, var=var

if n_params() eq 0 then begin
  print, 'Syntax:  Result = FUNDAMENTAL_FREQ( a, e, I, var= )'
  print, 'Returns the physical value of the mean motion (radians/sec) for an orbit with epicyclic elements a, e, and I,'
  print, 'accounting for Saturn''s j2, j4, j6, and j8.  '
  print, 'Set var=''kappa'' or var=''nu'' to calculate variables other than omega.'
  retall
endif

; Saturn mass, radius, and harmonics
saturn_constants, gm=gm, prad=prad, j2=j2, j4=j4, j6=j6, j8=j8
if not keyword_set(j8) then j8=0
if keyword_set(noj8) then j8=0
; Overwrite with input values, if they exist
if keyword_exists(_gm) then gm = _gm
if keyword_exists(_prad) then prad = _prad
if keyword_exists(_j2) then j2 = _j2
if keyword_exists(_j4) then j4 = _j4
if keyword_exists(_j6) then j6 = _j6
if keyword_exists(_j8) then j8 = _j8

; Find frequencies evaluated at epicyclic semimajor axis a
omega2_a = caviar_omega2( aa, gm=gm, prad=prad, $
                          j2=j2, j4=j4, j6=j6, j8=j8 )
kappa2_a = caviar_omega2( aa, var='kappa', gm=gm, prad=prad, $
                          j2=j2, j4=j4, j6=j6, j8=j8 )
nu2_a = caviar_omega2( aa, var='nu', gm=gm, prad=prad, $
                       j2=j2, j4=j4, j6=j6, j8=j8 )
eta2_a = caviar_omega2( aa, var='eta', gm=gm, prad=prad, $
                        j2=j2, j4=j4, j6=j6, j8=j8 )
chi2_a = caviar_omega2( aa, var='chi', gm=gm, prad=prad, $
                        j2=j2, j4=j4, j6=j6, j8=j8 )
beta2_a = caviar_omega2( aa, var='beta', gm=gm, prad=prad, $
                         j2=j2, j4=j4, j6=j6, j8=j8 )
delta2_a = caviar_omega2( aa, var='delta', gm=gm, prad=prad, $
                          j2=j2, j4=j4, j6=j6, j8=j8 )
lambda2_a = caviar_omega2( aa, var='lambda', gm=gm, prad=prad, $
                           j2=j2, j4=j4, j6=j6, j8=j8 )
omega_a = sqrt( omega2_a )
kappa_a = sqrt( kappa2_a )
nu_a = sqrt( nu2_a )
alpha_1a = 1.0d0/3 * ( 2*nu_a + kappa_a )
alpha_2a = 2*nu_a - kappa_a
alpha2_a = alpha_1a * alpha_2a

if not keyword_set(var) then var='omega'
case var of
  'omega': begin
    out = omega_a * (1+( 3.5d0 - 3*eta2_a/kappa2_a $
                         - kappa2_a/2/omega2_a )*ee^2 $
                      +( 2.0d0 - kappa2_a/2/omega2_a - $
                         3*chi2_a/2/kappa2_a )*ii^2 )
  end
  'kappa': begin
    out = kappa_a * (1+( -1.5d0 + 3*eta2_a/kappa2_a - $
                         15*eta2_a^2/4/kappa2_a^2 + 15*omega2_a/4/kappa2_a - $
                         3*beta2_a/2/kappa2_a )*ee^2 $
                      +( -1.5d0 + 3*eta2_a/kappa2_a - $
                         9*chi2_a*eta2_a/kappa2_a^2 - $
                         3*chi2_a^2/4/kappa2_a/alpha2_a + $
                         3*delta2_a/2/kappa2_a )*ii^2 )

  end
  'nu': begin
    out = nu_a * (1+( 3*chi2_a/2/nu2_a - 3*chi2_a^2/4/nu2_a/alpha2_a - $
                      9*eta2_a*chi2_a/4/kappa2_a/nu2_a + $
                      3*delta2_a/2/nu2_a )*ee^2 $
                   +( 3*chi2_a/2/nu2_a - 9*chi2_a^2/8/kappa2_a/nu2_a + $
                      3*chi2_a^2/16/nu2_a/alpha2_a - $
                      9*lambda2_a/16/nu2_a )*ii^2 )
  end
  else: begin
    stop, 'var unrecognized'
  end
endcase

return, out

end
