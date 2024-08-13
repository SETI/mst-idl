cap_n = 300
nn = findgen(cap_n)
kk = findgen(cap_n)
tt = nn/cap_n*12
deltat = tt[1]
deltaj = 0.125
s0 = 2*deltat
cap_j = (1/deltaj) * alog2( cap_n * deltat / s0 )
jj = findgen(cap_j+1)
ss = s0 * 2^( jj*deltaj )
omega = 2*!pi*kk/cap_n/deltat
;omega[where(kk gt cap_n/2)] = -omega[where(kk gt cap_n/2)]

; Calculate wavelet base functions as a cap_n-by-cap_j array
omega0 = 6
;psihat0 = !dpi^(-0.25) * exp( -( ss ## omega - omega0 )^2/2 )
;psihat0[where( omega le 0 ),*] = 0
;psihat = sqrt(2*!pi*(ss ## replicate(1,cap_n))/deltat) * psihat0
;w_delta = total(psihat,1) / cap_n
w_delta = dblarr(cap_j+1)
foo = where( kk le cap_n/2 )
for x=0,cap_j do w_delta[x] = !dpi^(-.25) * sqrt(2*!pi*ss[x]/deltat) / $
          cap_n * total(exp( -(ss[x]*omega[foo]-omega0)^2/2 ))
c_delta = total(w_delta/sqrt(ss)) * deltaj * sqrt(deltat) * !pi^0.25

;; This was an attempt to compare TC98's method to analytic equations.
;; It doesn't seem to have worked very well.  
;eta = fltarr( cap_n, cap_j+1, cap_n )
;for x=0,cap_n-1 do for y=0,cap_j do for z=0,cap_n-1 do begin
;  eta[x,y,z] = ( tt[z] - tt[x] )/ss[y]
;endfor
;psi0 = !pi^(-0.25) * exp(complex(0,1)*omega0*eta) * exp(-eta^2/2)
;psi = sqrt(deltat/ rebin((ss ## replicate(1,cap_n)),cap_n,cap_j+1,cap_n) ) * psi0
;
;xx = sin( tt * !pi / 2 )
;xxarray = rebin(rebin(xx,1,1,cap_n),cap_n,cap_j+1,cap_n)
;mywavelet = complexarr(cap_n,cap_j+1)
;for x=0,cap_n-1 do for y=0,cap_j do begin
;  mywavelet[x,y] = complex( $
;     int_tabulated( tt, xxarray[x,y,*]*real_part(conj(psi[x,y,*])) ), $
;     int_tabulated( tt, xxarray[x,y,*]*imaginary(conj(psi[x,y,*])) ) )
;endfor
;
;; Could specify s0, dj, and j, but automatically calculates same values I have.
;tcwavelet = wavelet( xx, deltat )


end
