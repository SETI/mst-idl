function hz_root, r0
  common h, hz, j2, j4, j6
  return, hz - r0^2*sqrt(caviar_omega2(r0,j2=j2,j4=j4,j6=j6))
end

function cart_to_epicyclic, _pos, _vel, debug=debug, verbose=verbose, convert=convert, mu=gm, prad=prad, musat=musat, noj4=noj4, noj6=noj6, noj8=noj8, use_hz=use_hz, tol=tol, r0=r0, np=np, q=q

if n_params() eq 0 then begin
  print, 'Syntax:  Result = CART_TO_EPICYCLIC( pos, vel )'
  print, 'Calculates epicyclic elements from Cartesian coordiantes, using the method'
  print, 'of Renner and Sicardy (2006)'
  print, 'All distances are in KM.  All velocities are in KM PER SECOND.'
endif

; Get Saturn mass and radius and J2
common h, hz, j2, j4, j6
if keyword_set(musat) then saturn_constants, gm=gm, prad=prad, $
                                             j2=j2, j4=j4, j6=j6, j8=j8
if keyword_set(noj4) then j4 = 0 & j6 = 0 & j8 = 0
if keyword_set(noj6) then j6 = 0 & j8 = 0
if keyword_set(noj8) then j8 = 0
if not keyword_set(gm) then stop, 'Must specify central mass mu'
if not keyword_set(prad) then stop, 'Must specify central body radius prad'
if not keyword_set(j2) then stop, 'Must specify gravity harmonic J2'

pos = _pos
vel = _vel
; By default, use the vertical angular momentum to calculate a
if not keyword_exists(use_hz) then use_hz = 1

; Unpack Cartesian coordinates from the arrays pos and vel
xx = pos[*,0]
yy = pos[*,1]
zz = pos[*,2]
xdot = vel[*,0]
ydot = vel[*,1]
zdot = vel[*,2]
nx = n_elements(xx)

; Calculate cylindrical coordinates, from RS06 Eqs 22 thru 29
rr = sqrt( xx^2 + yy^2 )
ll = atan( yy / xx )
; If x=0, manually set ll to pi/2 or 3pi/2, depending on whether y>0 or not. 
foo = where( xx eq 0, count )
if count gt 0 then begin
  ll[foo] = !dpi/2
  foo1 = where( yy[foo] lt 0, count1 )
  if count1 gt 0 then ll[foo[foo1]] = 3*!dpi/2
endif
; If x<0, advance ll by pi. 
foo = where( xx lt 0, count )
if count gt 0 then ll[foo] = ll[foo] + !dpi
; Make sure ll lies between 0 and 2pi. 
ll = fix_angles( ll, /rad, /to360 )
; Calculate cylindrical velocities, from RS06 Eqs 24 and 25. 
rdot = xdot*cos(ll) + ydot*sin(ll)
ldot = ( ydot*cos(ll) - xdot*sin(ll) )/rr

; Calculate vertical component of angular momentum, use it to get r0
; From RS06 Eqs 48 thru 50
if not keyword_exists(verbose) and nx gt 10000 then verbose = 1
r0 = dblarr(nx)
_hz = xx*ydot - yy*xdot
if keyword_set(findfile('r0.sav')) then begin
  print, 'Restoring r0.sav...'
  restore, 'r0.sav'
endif else for j=0l,nx-1 do begin
  if keyword_set(verbose) and j eq 0 then print, 'Finding r0...'
  if j ne 0 and j mod 100000 eq 0 then begin
    print, 'j = '+strtrim(j,2)+' / '+strtrim(nx,2)
  endif
  r00 = (v_mag(pos[j,*]))[0] + [ -5, 0, 5 ]
  hz = _hz[j]
  if finite(hz) then if hz gt 0 then begin
    r0[j] = fx_root( r00, 'hz_root', /double )
  endif
endfor

; Initialize variables
aa = rr
aa_last = aa
ee = replicate( 0.0d0, nx )
ii = replicate( 0.0d0, nx )
omega = replicate( 0.0d0, nx )
pomega = replicate( 0.0d0, nx )
lambda = replicate( 0.0d0, nx )
rc = replicate( 0.0d0, nx )
lc = replicate( 0.0d0, nx )
zc = replicate( 0.0d0, nx )
rdotc = replicate( 0.0d0, nx )
ldotc = replicate( 0.0d0, nx )
zdotc = replicate( 0.0d0, nx )
err = replicate( 0.0d0, nx )
nstep = 0l
if not keyword_set(maxstep) then maxstep = 50l
; Define q, the index of particles still being iterated
; If r0 is identically zero, then epicyclic elements cannot be found for 
; this particle; remove it from the index at the beginning
q = where( r0 ne 0, count )
if count eq 0 then return, replicate( 0.0d0, nx, 6 )
err[q] = 1.0d5
done = where( err gt err+1, ndone )

; Iteratively find epicyclic elements
while ndone lt nx and nstep lt maxstep do begin

  if keyword_set(verbose) then begin
    print, 'Step '+strtrim(nstep+1,2)+'; finished '+strtrim(ndone,2)+$
           ' particles out of '+strtrim(nx,2)+'.'
  endif
  if keyword_set(debug) then stop

  ; Calculate frequencies, from MD99 Eqs 6.244 thru 6.246 
  ; and BL94 Eqs A1 thru A8 (superseding RS06 Eqs 14 thru 21)
  freq_names = [ 'kappa', 'nu', 'eta', 'chi', 'beta', 'delta', 'lambda' ]
  nfreqs = n_elements(freq_names)
  nn2_a = caviar_omega2( aa[q], gm=gm, prad=prad, j2=j2, j4=j4, j6=j6, j8=j8 )
  nn_a = sqrt(nn2_a)
  for j=0,nfreqs-1 do begin
    ;kappa2_a = caviar_omega2( aa[q], var='kappa', gm=gm, prad=prad, $
    ;                          j2=j2, j4=j4, j6=j6, j8=j8 )
    foo = execute( freq_names[j]+'2_a = caviar_omega2( aa[q], var='''+$
                   freq_names[j]+$
                   ''', gm=gm, prad=prad, j2=j2, j4=j4, j6=j6, j8=j8 )' )
    ;kappa_a = sqrt(kappa2_a)
    foo = execute( freq_names[j]+'_a = sqrt('+freq_names[j]+'2_a)' )
  endfor
  alpha_1a = 1.0d0/3 * ( 2*nu_a + kappa_a )
  alpha_2a = 2*nu_a - kappa_a
  alpha2_a = alpha_1a * alpha_2a

  ; Find new values of epicyclic elements, from RS06 Eqs 42 thru 47
  aa_last[q] = aa[q]
  aa[q] = (rr[q]-rc[q])/( 1 - (ldot[q]-ldotc[q]-nn_a)/2/nn_a )
  ee[q] = sqrt( ((ldot[q]-ldotc[q]-nn_a)/2/nn_a)^2 + $
                ((rdot[q]-rdotc[q])/aa[q]/kappa_a)^2 )
  ii[q] = sqrt( ((zz[q]-zc[q])/aa[q])^2 + ((zdot[q]-zdotc[q])/aa[q]/nu_a)^2 )
  if keyword_set(use_hz) then aa[q] = r0[q]*( 1 + ee[q]^2 + ii[q]^2 )
  lambda[q] = ll[q] - lc[q] - 2*nn_a/kappa_a*(rdot[q]-rdotc[q])/aa[q]/kappa_a
  tan_mean_anom = (rdot[q]-rdotc[q])/aa[q]/kappa_a/( 1 - (rr[q]-rc[q])/aa[q] )
  mean_anom = atan( tan_mean_anom )
  foo = where( rr[q] gt aa[q], count )
  if count gt 0 then mean_anom[foo] = mean_anom[foo] + !dpi
  pomega[q] = fix_angles( lambda[q] - mean_anom, /rad, /to360 )
  mean_arg = atan( nu_a*(zz[q]-zc[q])/(zdot[q]-zdotc[q]) )
  foo = where( zdot[q]-zdotc[q] eq 0, count )
  if count gt 0 then mean_arg[foo] = lambda[q[foo]]
  foo = where( ( zz[q] gt 0 and mean_arg lt 0 ) or $
               ( zz[q] lt 0 and mean_arg gt 0 ), count )
  if count gt 0 then mean_arg[foo] =  mean_arg[foo] + !dpi
  omega[q] = fix_angles( lambda[q] - mean_arg, /rad, /to360 )

  ; Find new second-order components of epicyclic elements,
  ; from RS06 Eqs 36 thru 41
  rc[q] = aa[q]*ee[q]^2*( 1.5d0*eta2_a/kappa2_a - 1 $
                          - eta2_a/2/kappa2_a*cos(2*(lambda[q]-pomega[q])) ) + $
          aa[q]*ii[q]^2*( 0.75d0*chi2_a/kappa2_a - 1 $
                          + chi2_a/4/alpha2_a*cos(2*(lambda[q]-omega[q])) )
  lc[q] = ee[q]^2*(0.75d0+eta2_a/2/kappa2_a)*nn_a/kappa_a*$
                                                 sin(2*(lambda[q]-pomega[q])) $
          - ii[q]^2*chi2_a/4/alpha2_a*nn_a/nu_a*sin(2*(lambda[q]-omega[q]))
  zc[q] = aa[q]*ii[q]*ee[q]*( chi2_a/2/kappa_a/alpha_1a*$
                                       sin(2*lambda[q]-pomega[q]-omega[q]) $
                              - 1.5d0*chi2_a/kappa_a/alpha_2a*$
                                       sin(pomega[q]-omega[q]) )
  rdotc[q] = aa[q]*ee[q]^2*eta2_a/kappa_a*sin(2*(lambda[q]-pomega[q])) - $
             aa[q]*ii[q]^2*chi2_a/2/alpha2_a*nu_a*sin(2*(lambda[q]-omega[q])) 
  ldotc[q] = ee[q]^2*nn_a*( 3.5d0 - 3.0d0*eta2_a/kappa2_a - kappa2_a/2/nn_a^2 $
                      + (1.5d0+eta2_a/kappa2_a)*cos(2*(lambda[q]-pomega[q])) ) $
            + ii[q]^2*nn_a*( 2.0d0 - 1.5d0*chi2_a/kappa2_a - kappa2_a/2/nn_a^2 $
                      - chi2_a/2/alpha2_a*cos(2*(lambda[q]-omega[q])) )
  zdotc[q] = aa[q]*ii[q]*ee[q]*( chi2_a*(kappa_a+nu_a)/2/kappa_a/alpha_1a*$
                                          cos(2*lambda[q]-pomega[q]-omega[q]) $
                               + 1.5d0*chi2_a*(kappa_a-nu_a)/kappa_a/alpha_2a*$
                                          cos(pomega[q]-omega[q]) )

  ; Check convergence
  err = abs( aa - aa_last )
  if nstep eq 0 then begin
    tol = aa*( ee^3 + ii^3 )/5
  endif else if nstep lt 15 then begin
    tol[q] = aa[q]*( ee[q]^3 + ii[q]^3 )/5
  endif else if nstep eq 15 then begin
    print, 'Adjusting tolerance from a*(e^3+I^3)/5 to a*(e^3+I^3)...'
    tol[q] = aa[q]*( ee[q]^3 + ii[q]^3 )
  endif else if nstep ge 15 and nstep lt 25 then begin
    tol[q] = aa[q]*( ee[q]^3 + ii[q]^3 )
  endif else if nstep eq 25 then begin
    print, 'Adjusting tolerance from a*(e^3+I^3) to a*(e^3+I^3)*3...'
    tol[q] = aa[q]*( ee[q]^3 + ii[q]^3 ) * 3
  endif else begin
    tol[q] = aa[q]*( ee[q]^3 + ii[q]^3 ) * 3
  endelse
  ndone_last = ndone
  done = where( err lt tol, ndone )
  if nstep eq 0 then oldnd = ndone else oldnd = [ oldnd, ndone ]
  if ndone gt 0 and ndone lt nx then q = vec_remove( lindgen(nx), done )
  ;if ndone eq ndone_last then stop

  nstep = nstep + 1

endwhile
print, strtrim(ndone,2)+' of '+strtrim(nx,2)+' particles converged'
       ;' with a tolerance of '+strtrim(tol,2)+' km'
print, 'after '+strtrim(nstep,2)+' steps (max='+strtrim(maxstep,2)+').'

out = [ [aa], [ee], [ii], [omega], $
        [fix_angles(pomega-omega,/rad,/to360)], $
        [fix_angles(lambda-pomega,/rad,/to360)] ]
return, out

end
