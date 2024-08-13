pro fit_wavelet, flip=flip, xx0=xx0, xx1=xx1, xr=xr, mm=mm, cutoff=cutoff, $
        rrshift=rrshift, dolzr=dolzr, p0=p0, rsat=rsat, lambdapic=lambda, $
        lambdasat=lambdasat, noise=noise, nbfig=nbfig, resname=resname

common wavelet10

if !d.name eq 'X' then begin
  winset, 1, nowin
  if keyword_set(nowin) then window, 1, xs=1280, ys=1024
endif
!p.font = 1
!p.charsize = 1.5
!p.thick = 2
if keyword_set(dolzr) then begin
  if not keyword_set(psname) then psname = 'plot'
;  lzr, psname
  plot_color
endif
yma = [4,4]
if not keyword_set(flip) then flip = .25

if not keyword_set(nbfig) then begin
  !p.multi=0
  !p.multi=[0,2,2]
  plot, rr, vv, /xs, /ys, xtit=xtit, ytit=ytit, tit='Raw Wave', yma=yma
endif

if param eq 6 then cdelta = 0.776
psi0 = !pi^(-0.25)
suse = where( period lt cutoff, count )
if count eq n_elements(period) then begin
  nvv = n_elements(vv)
  lowpass = interpol( [ mean(vv[0:10]), mean(vv[nvv-11:nvv-1]) ], rr[[5,nvv-6]], rr )
  highpass = vv - lowpass
endif else begin
  highpass = dj*sqrt(dt)/(cdelta*psi0)*( float(wave[*,suse]) # (1./sqrt(scale[suse])) )
  lowpass = vv - highpass
endelse

totphase = unwrap_phase(get_phase( total(wave[*,suse],2) ))
int1 = findgen(xx1-xx0) + xx0
fit1 = poly_fit(rr[int1],totphase[int1],2)
zpt1 = -fit1[1]/fit1[2]/2
rres = zpt1 + thoukm
_phase0 = poly(zpt1,fit1)
phase0 = (_phase0+180*flip) mod 360
sigma = 2*!pi/3.08/rres^4*60330.^4*(mm-1)/(fit1[2]*2*!pi/180)
tit1 = 'Fit Quadratic to Wavelet Phase'
;tit1 = tit1 + '!Crres = '+string(rres,fo='(F8.1)')+' km, phase = '+$
;      string(phase0,fo='(I3)')+'!Eo!N, sigma = '+string(sigma,fo='(F4.1)')+$
;      ' g/cm!U2!N'
plot, rr, totphase, xr=xr, /xs, /ys, ytickf = '(I6)', xtit=xtit, ytit='Phase (degrees)', yma=yma, tit=tit1
oplot, rr[int1], totphase[int1], co=red()
oplot, rr, poly(rr,fit1), co=cyan()
oplot, [zpt1], [_phase0], ps=4, co=cyan()
if not keyword_set(resname) then resname=''
xyouts, xr[0]+(xr[1]-xr[0])*.03, !y.crange[1]-(!y.crange[1]-!y.crange[0])*.075, 'rres = '+string(rres,fo='(F8.1)')+' km!Cphase = '+string(round(phase0),fo='(I3)')+'!Eo!N!C(expected phase = '+string(round(( -mm*(lambda-lambdasat) + 360*mm ) mod 360),fo='(I3)')+'!Eo!N)!Csigma = '+string(sigma,fo='(F4.1)')+' g/cm!U2!N',charsize=!p.charsize/2
axis, yaxis=0, ytickn=replicate(' ',20), yticklen=1e-10, ytit=resname+'!C', charsize=!p.charsize*2

common fdensity_wave33, which_params, _m, _lambda, _lambdasat, _rsat, _thoukm, _rres, _r, _y, _rres1, _sigma1
x0 = min(where(rr gt zpt1)) + rrshift
_r = rr[x0:xx1]
_y = highpass[x0:xx1]
_m = mm
_lambdasat = 0
_lambda = - phase0*!pi/180 / mm
_thoukm = thoukm
_rres1 = rres
_sigma1 = sigma * 1e10

ftol = 1e-2
if keyword_set(useamoeba) then begin
  which_params = [ 1, 1, 1, 1, 0, 1 ]
  p0 = [ (max(highpass)-min(highpass))/8, _sigma1, _rres1, -_m*(_lambda-_lambdasat), 0, 7.5 ]
  which_params = [ 1, 0, 0, 0, 0, 1 ]
  ;p0 = [ (max(highpass)-min(highpass))/8, 0, 0, 0, 0, 7.5 ]
  scal = [ 2*p0[0], 4e11, 10, !pi, 0, p0[5] ]
  pp = amoeba( ftol, scale=scal, p0=p0, function_value=fval, function_name='fdensity_wave3' )
  pp_sigma = [0,0]
endif else if keyword_set(uselinear) then begin
  fdensity_wave4, _r, [1,0], base, xi=xi
  fit = poly_fit( xi^3, alog(abs(_y/base)), 1 )
  pp = [ exp(fit[0]), (-1./fit[1])^(1./3) ]
  pp_sigma = [0,0]
endif else begin
  if not keyword_set(p0) then p0 = [ (max(highpass)-min(highpass))/8, 7.5 ]
  pp = p0
  weights = replicate(1.,n_elements(_y))
;  if keyword_set(noise) then begin
;    err = stddev(highpass[noise[0]:noise[1]])
;    weights = replicate(1./err^2,n_elements(_y))
;  endif
  yfit = mpcurvefit( _r, _y, weights, pp, pp_sigma, function_name='fdensity_wave4', iter=iter, chisq=chisq, /double, yerror=yerror )
endelse

cap_g = 6.672e-23  ;km^3/g/s^2
gm = 37931289.4836 ;km^3/s^2 for Saturn
alpha = rres / rsat
lapl = laplace( alpha, 0, mm, 1 )
satmass = [pp[0],pp_sigma[0]]/mean(vv[xx0:xx1]) * $
    ( 2 * sqrt(!dpi) * rsat * rres * sigma * 1e10 ) / $
    ( lapl[0,mm,1] + 2*mm*lapl[0,mm,0] ) / gm * cap_g
print, 'Satellite mass: '+strtrim(satmass[0],2)+' +- '+strtrim(satmass[1],2)
print, 'Damping: '+strtrim(pp[1],2)+' +- '+strtrim(pp_sigma[1],2)
tit2 = 'High-Pass Filter, then fit density wave
;tit2 = 'High-Pass Filter (done via wavelet), then fit density wave
;tit2 = tit2 + '!CAmplitude = '+strtrim(pp[0],2)+', Damping = '+strtrim(pp[1],2)
plot, rr, highpass, xr=xr, /xs, ys=16, xtit=xtit, ytit=ytit, yma=yma, tit=tit2
oplot, _r, highpass[x0:xx1], co=red()
if keyword_set(useamoeba) then foo = fdensity_wave3( pp, f=f ) else fdensity_wave4, _r, pp, f
oplot, _r, f, co=green()
;'Amplitude = '+string(pp[0],fo='(F5.3)')+'
xyouts, xr[0]+(xr[1]-xr[0])*.03, !y.crange[1]-(!y.crange[1]-!y.crange[0])*.075, 'Moon mass = '+strsn(satmass[0],ndec=1,/d2)+' M!DS!N!CDamping = '+string(pp[1],fo='(F4.2)'),charsize=!p.charsize/2

_r = rr[x0-rrshift:xx1]
_y = highpass[x0-rrshift:xx1]
if keyword_set(useamoeba) then foo = fdensity_wave3( pp, f=f ) else fdensity_wave4, _r, pp, f
plot, rr, vv, /xs, ys=16, xr=xr, xtit=xtit, ytit=ytit, yma=yma, tit='Compare Fit to Raw Wave'
oplot, rr, vv
oplot, _r, f+lowpass[x0-rrshift:xx1], co=green()

print, 'Expected Phase:  '+strtrim(( -mm*(lambda-lambdasat) + 360*mm ) mod 360,2)

if not keyword_set(dolzr) and not keyword_set(nbfig) then stop

end
