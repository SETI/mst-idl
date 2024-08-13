; Create frames that could be assembled into a movie showing how each 
; parameter in linear density wave theory influences wave morphology. 

xi_d0 = 10.
mm = 10
rres0 = 1.3d5
a0 = 1
phi0 = 0
sigma0 = 40.
dr1 = 155
dr2 = 15
nr = 1500
radi = findgen(nr)/nr*dr1 + rres0 - dr2
val = fdensity_wave5( radi, a=a0, xi_d=xi_d0, mm=mm, phi=phi0, $
                          rres=rres0, sigma=sigma0, xiout=xiout )

if keyword_set(dolzr) then begin
  lzr, 'density_wave_movie_base', /port
  @plot_prepare
  !p.charsize = 2
endif
!p.multi=[0,1,6]
plot, radi-rres0, val, /xs, /ys, yticki=5, xtickn=replicate(' ',20), $
      ytit='!MD!Ms(r)'
oplot, [0,0], !y.crange, l=1
if keyword_set(dolzr) then clzr
if keyword_set(baseonly) then stop

;dsigma = 5
;nsigma = 4
dsigma = 1
nsigma = 20
for j=1,nsigma do begin
  sigma = sigma0 - dsigma*j
  xi_d = xi_d0 * sqrt(40/sigma)
  val = fdensity_wave5( radi, a=a0, xi_d=xi_d, mm=mm, phi=phi0, $
                        rres=rres0, sigma=sigma, xiout=xiout )
  if keyword_set(dolzr) then begin
    lzr, 'density_wave_movie_sigma'+string(sigma,fo='(I2)'), /port
  endif
  !p.multi=[0,1,6]
  plot, radi-rres0, val, /xs, /ys, yticki=5, xtickn=replicate(' ',20), $
        ytit='!MD!Ms(r)'
  oplot, [0,0], !y.crange, l=1
  if keyword_set(dolzr) then clzr
endfor

drres = .5
nrres = 20
for j=1,nrres do begin
  rres = rres0 - drres*j
  val = fdensity_wave5( radi, a=a0, xi_d=xi_d0, mm=mm, phi=phi0, $
                        rres=rres, sigma=sigma0, xiout=xiout )
  if keyword_set(dolzr) then begin
    if rres0-rres ge 10 then nn='5' else nn='4'
    lzr, 'density_wave_movie_rres'+string(rres0-rres,fo='(F'+nn+'.2)'), $
         /port
  endif
  !p.multi=[0,1,6]
  plot, radi-rres0, val, /xs, /ys, yticki=5, xtickn=replicate(' ',20), $
        ytit='!MD!Ms(r)'
  oplot, [0,0]+rres-rres0, !y.crange, l=1
  if keyword_set(dolzr) then clzr
endfor

dxi_d = -.25
nxi_d = 20
for j=1,nxi_d do begin
  xi_d = xi_d0 - dxi_d*j
  val = fdensity_wave5( radi, a=a0, xi_d=xi_d, mm=mm, phi=phi0, $
                        rres=rres0, sigma=sigma0, xiout=xiout )
  if keyword_set(dolzr) then begin
    lzr, 'density_wave_movie_xi_d'+string(xi_d,fo='(F5.2)'), /port
  endif
  !p.multi=[0,1,6]
  plot, radi-rres0, val, /xs, /ys, yticki=5, xtickn=replicate(' ',20), $
        ytit='!MD!Ms(r)'
  oplot, [0,0], !y.crange, l=1
  if keyword_set(dolzr) then clzr
endfor

da = .04
na = 20
for j=1,na do begin
  a = a0 - da*j
  val = fdensity_wave5( radi, a=a, xi_d=xi_d0, mm=mm, phi=phi0, $
                        rres=rres0, sigma=sigma0, xiout=xiout )
  if keyword_set(dolzr) then begin
    lzr, 'density_wave_movie_a'+string(a,fo='(F4.2)'), /port
  endif
  !p.multi=[0,1,6]
  plot, radi-rres0, val, /xs, /ys, yticki=5, xtickn=replicate(' ',20), $
        ytit='!MD!Ms(r)', yr=!y.crange
  oplot, [0,0], !y.crange, l=1
  if keyword_set(dolzr) then clzr
endfor

dphi = -18
nphi = 19
for j=1,nphi do begin
  phi = phi0 - dphi*j
  val = fdensity_wave5( radi, a=a0, xi_d=xi_d0, mm=mm, phi=phi, $
                        rres=rres0, sigma=sigma0, xiout=xiout )
  if keyword_set(dolzr) then begin
    lzr, 'density_wave_movie_phi'+strtrim(phi,2), /port
  endif
  ;!p.multi=[0,1,6]
  plot, radi-rres0, val, /xs, /ys, yticki=5, xtickn=replicate(' ',20), $
        ytit='!MD!Ms(r)'
  oplot, [0,0], !y.crange, l=1
  if keyword_set(dolzr) then clzr
endfor

end
