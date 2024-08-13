function epicyclic_to_cart, epicyclic, vel=vel, debug=debug, convert=convert, mu=gm, prad=prad, musat=musat, noj4=noj4, noj6=noj6, noj8=noj8

if n_params() eq 0 then begin
  print, 'Syntax:  Result = EPICYCLIC_TO_CART( epicyclic, vel=vel )'
  print, 'Converts epicyclic elements into Cartesian coordinates, using the method'
  print, 'of Renner and Sicardy (2006)'
  print, 'All distances are in KM.  All angles are in RADIANS.'
endif

; Get Saturn mass and radius and J2
if keyword_set(musat) then saturn_constants, gm=gm, prad=prad, $
                                             j2=j2, j4=j4, j6=j6, j8=j8
if keyword_set(noj4) then j4 = 0 & j6 = 0 & j8 = 0
if keyword_set(noj6) then j6 = 0 & j8 = 0
if keyword_set(noj8) then j8 = 0
if not keyword_set(gm) then stop, 'Must specify central mass mu'
if not keyword_set(prad) then stop, 'Must specify central body radius prad'
if not keyword_set(j2) then stop, 'Must specify gravity harmonic J2'

; Unpack epicyclic elements from the array epicyclic
aa = epicyclic[*,0]
ee = epicyclic[*,1]
ii = epicyclic[*,2]
omega = epicyclic[*,3]
pomega = epicyclic[*,4] + omega
lambda = epicyclic[*,5] + pomega

; Calculate frequencies, from MD99 Eqs 6.244 thru 6.246 
; (superseding RS06 Eqs 14 thru 21)
freq_names = [ 'kappa', 'nu', 'eta', 'chi', 'beta', 'delta', 'lambda' ]
nfreqs = n_elements(freq_names)
nn2_a = caviar_omega2( aa, gm=gm, prad=prad, j2=j2, j4=j4, j6=j6, j8=j8 )
nn_a = sqrt(nn2_a)
for j=0,nfreqs-1 do begin
  ;kappa2_a = caviar_omega2( aa, var='kappa', gm=gm, prad=prad, $
  ;                          j2=j2, j4=j4, j6=j6, j8=j8 )
  foo = execute( freq_names[j]+'2_a = caviar_omega2( aa, var='''+$
                 freq_names[j]+$
                 ''', gm=gm, prad=prad, j2=j2, j4=j4, j6=j6, j8=j8 )' )
  ;kappa_a = sqrt(kappa2_a)
  foo = execute( freq_names[j]+'_a = sqrt('+freq_names[j]+'2_a)' )
endfor
alpha_1a = 1.0d0/3 * ( 2*nu_a + kappa_a )
alpha_2a = 2*nu_a - kappa_a
alpha2_a = alpha_1a * alpha_2a

; Calculate cylindrical coordinates, from RS06 Eqs 2 thru 7
rr = aa*( 1 $
          - ee*cos(lambda-pomega) $
          + ee^2*( 1.5d0*eta2_a/kappa2_a - 1 $
                   - eta2_a/2/kappa2_a*cos(2*(lambda-pomega)) ) $
          + ii^2*( 0.75d0*chi2_a/kappa2_a - 1 $
                   + chi2_a/4/alpha2_a*cos(2*(lambda-omega)) ) )
ll = lambda + 2*ee*nn_a/kappa_a*sin(lambda-pomega) $
     + ee^2*( 0.75d0 + eta2_a/2/kappa2_a )*nn_a/kappa_a*sin(2*(lambda-pomega)) $
     - ii^2*chi2_a/4/alpha2_a*nn_a/nu_a*sin(2*(lambda-pomega))
zz = aa*ii*( sin(lambda-omega) $
             + ee*chi2_a/2/kappa_a/alpha_1a*sin(2*lambda-pomega-omega) $
             - 1.5d0*ee*chi2_a/kappa_a/alpha_2a*sin(pomega-omega) )
rdot = aa*kappa_a*( ee*sin(lambda-pomega) $
                   + ee^2*eta2_a/kappa2_a*sin(2*(lambda-pomega)) $
                   - ii^2*chi2_a/2/alpha2_a*nu_a/kappa_a*sin(2*(lambda-omega)) )
ldot = nn_a*( 1 $
              + 2*ee*cos(lambda-pomega) $
              + ee^2*( 3.5d0 - 3.0d0*eta2_a/kappa2_a - kappa2_a/2/nn_a^2 $
                       + ( 1.5d0 + eta2_a/kappa2_a )*cos(2*(lambda-pomega)) ) $
              + ii^2*( 2.0d0 - 1.5d0*chi2_a/kappa2_a - kappa2_a/2/nn_a^2 $
                       - chi2_a/2/alpha2_a*cos(2*(lambda-omega)) ) )
zdot = aa*ii*nu_a*( cos(lambda-omega) $
                    + ee*chi2_a*(kappa_a+nu_a)/2/kappa_a/alpha_1a/nu_a*$
                                        cos(2*lambda-pomega-omega) $
                    + 1.5d0*ee*chi2_a*(kappa_a-nu_a)/kappa_a/alpha_2a/nu_a*$
                                        cos(pomega-omega) )

; Calculate Cartesian coordinates, from RS06 Eqs 8 thru 13
xx = rr*cos(ll)
yy = rr*sin(ll)
xdot = rdot*cos(ll) - rr*ldot*sin(ll)
ydot = rdot*sin(ll) + rr*ldot*cos(ll)

pos = [ [xx], [yy], [zz] ]
vel = [ [xdot], [ydot], [zdot] ]

if keyword_set(debug) then stop

return, pos

end
