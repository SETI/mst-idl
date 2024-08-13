if not keyword_exists(do13e5) then do13e5 = 1
savefile = '/home/borogove/matthewt/idl/iss/soirings/soirings_some_wave_046rdhresscn'
if keyword_set(do1e5) then savefile = savefile + '.1e5'
if keyword_set(do13e5) then savefile = savefile + '.13e5'
savefile = savefile + '.sav'
if not keyword_set(findfile(savefile)) then begin

  if keyword_set(do13e5) then begin
    xi_d = 10.
    mm = 10
    rres = 1.3d5
  endif else if keyword_set(do1e5) then begin
    xi_d = 10.
    mm = 10
    rres = 1e5
  endif else begin
    xi_d = 7.
    mm = 27
    rres = resloc(27,26,618)  ; Pan 27:26 at 130294.77 km
  endelse
  a = 1
  phi = 0
  sigma = 42.

  @soirings_some_wave_get_savefile_046rdhresscn

endif else restore, savefile

if not keyword_set(noplot) then begin
  if not keyword_exists(dir) then dir='/home/borogove/matthewt/latex/soirings/graphics/'
  !p.charsize = 1
  if keyword_set(dolzr) then begin
    lzr, dir+'soirings_some_wave1'
    @plot_prepare
  endif else window, 4
  pm = ' !S+!R!M^ ';' +- '
  xtit='r - r!DL!N (km)';'Distance from r!DL!N (km)'
  solid_diamonds
endif
if not keyword_exists(nosqrt) then nosqrt = 0
if keyword_set(do13e5) then begin
  minlam = 1.6
endif else if keyword_set(do1e5) then begin
  minlam = 1
endif else begin
  minlam = 2
endelse
run_wavelet, radi-rres, val, wave, wavenum, minlam=minlam, errbar=errbar, $
             yticki=5, nosqrt=nosqrt, totphase=totphase, zeroes=zeroes, $
             xtit=xtit, runits='km', ytit='!MD!Ms(r) = x(r)', noplot=noplot, $
             /phi, maxlam=50, /phasepanel, pkt=0, levels=wavelet_levels, $
             specify=dolzr, equal=0
if keyword_set(intridgepower) then begin
  rlv = 15
  wavelet_contour, wave, radi-rres, wavenum, wavelet_levels[rlv], nosqrt=nosqrt
endif
if not keyword_set(noplot) then begin
  oplot, [0,0], [!y.crange[0], !y.crange[1]*1.5-!y.crange[0]*.5], l=1, /noclip
  oplot, radi-rres, k_to_sigma(mm,rres) / sigma * (radi-rres), l=2
endif

if not keyword_exists(highest) then highest = 1
ridges = wavelet_ridges( wave, radi, wavenum, highest=highest, $
                         minpower=10.^(wavelet_levels[0]), $
                         /gauss, ridgepower=ridgepower5 )
if not keyword_set(noplot) then begin
  if keyword_set(highest) then begin
    oplot, ridges[*,2] - rres, ridges[*,3]
  endif else begin
    ridges1 = where( ridges[*,2]-rres gt 40 and ridges[*,3] lt 0.5, count )
    ridges2 = vec_remove( lindgen(n_elements(ridges[*,0])), ridges1 )
    oplot, ridges[ridges1,2] - rres, ridges[ridges1,3], co=red()
    oplot, ridges[ridges2,2] - rres, ridges[ridges2,3], co=red()
  endelse
endif
if keyword_set(fit_xi_d_only) then goto, fit_xi_d_only1

totphaseu = unwrap_phase( totphase )
;; The derivative of the phase produces a very wobbly curve, not worth plotting.
;oplot, radi-rres, deriv(totphaseu)/2/!dpi, co=green()
thwn = k_to_sigma(mm,rres) / sigma * (radi-rres)
if keyword_set(do13e5) then begin
  int1 = where( radi-rres ge 60 and radi-rres le 110 )
  if keyword_set(noise) then begin
    int1 = where( radi-rres ge 60 and radi-rres le 100 )
    if noise ge .25 then int1 = where( radi-rres ge 60 and radi-rres le 90 )
  endif
endif else if keyword_set(do1e5) then begin
  int1 = where( radi-rres ge 30 and radi-rres le 70 )
endif else begin
  int1 = where( radi-rres ge 60 and radi-rres le 110 )
endelse
if keyword_set(findfile('effscale.sav')) then begin
  restore, 'effscale.sav'
  fac = sqrt(effscale)
endif else fac = 1
fit1 = svdfit( radi[int1]-rres, ridges[int1,3], 2, chisq=chisq, sigma=pfsig1 )
;fit1 = poly_fit( radi[int1]-rres, ridges[int1,3], 1 )
zpt1 = -fit1[0]/fit1[1]  ; Curve goes to zero
rres1 = zpt1 + rres
zpt1x = where( abs(radi-rres1) eq min(abs(radi-rres1)) )
sigma1 = k_to_sigma(mm,rres1)/fit1[1]
sigma_rres1 = ( pfsig1[0]/fit1[1] )^2 + ( pfsig1[1]*fit1[0]/fit1[1]^2 )^2
sigma_rres1 = sqrt(sigma_rres1) * fac
sigma_sigma1 = ( 4*sigma_rres1/rres1 )^2 + ( pfsig1[1]/fit1[1] )^2
sigma_sigma1 = k_to_sigma(mm,rres1)/fit1[1] * sqrt(sigma_sigma1) * fac
;sigma_rres1 = 0.01
;sigma_sigma1 = 0.3

thphase = phi + xiout^2/2*180/!dpi - 45
if keyword_set(do13e5) then begin
  int2 = where( radi-rres ge 30 and radi-rres le 110 )
  if keyword_set(noise) then begin
    int2 = where( radi-rres ge 30 and radi-rres le 100 )
    if noise ge .17 then int2 = where( radi-rres ge 30 and radi-rres le 90 )
    if noise ge .5 then int2 = where( radi-rres ge 30 and radi-rres le 80 )
    if noise ge 2 then int2 = where( radi-rres ge 20 and radi-rres le 70 )
  endif
endif else if keyword_set(do1e5) then begin
  int2 = where( radi-rres ge 20 and radi-rres le 60 )
endif else begin
  int2 = where( radi-rres ge 30 and radi-rres le 110 )
endelse

; Use SVDFIT to do the quadratic fit.  Note that, unlike POLY_FIT, SVDFIT asks
; for the number of coefficients, which is 3 for a quadratic.  SVDFIT gives
; more trustworthy values for the error bars on the coefficients.  
; Also, when calculating error bars, note that SVDFIT automatically multiplies
; the 1-sigma uncertainty by sqrt(chisq/(n-m)), and returns this in the
; keyword "sigma" when there are no error bars on the input data values.
; Thus, pfsig2 are already correct error bars on the fit parameters.
fit2 = svdfit( radi[int2]-rres, totphaseu[int2], 3, chisq=chisq, sigma=pfsig2 )
if abs(fit2[0]+45-phi) gt 180 then begin
  adjust = round(( fit2[0]+45 - phi )/360) * 360
  print, 'Adjusting fitted phase', fit2, adjust
  fit2[0] = fit2[0] - adjust
  totphaseu = totphaseu - adjust
endif
if keyword_set(constrainphase) then begin
  ; Use the SVDFIT result as the starting point for a fit that holds the
  ; phase constant at the known input value.  
  fit2a = fit2
  pfsig2a = pfsig2
  ppq = fit2[1:2]
  weights = replicate( 1., n_elements(int2) )
  foo = mpcurvefit( radi[int2]-rres, $
                    totphaseu[int2] - phi + 45, $
                    weights, ppq, ppq_sigma, $
                    function_name='quadratic_holdzero', /quiet, $
                    iter=iterq, chisq=chisqq, /double, yerror=yerrorq )
  dofq = n_elements(int2) - 2
  fit2 = [ ppq[0]^2/4/ppq[1] + phi - 45, ppq[0], ppq[1] ]
  pfsig2 = [ 0, ppq_sigma * sqrt( chisqq / dofq ) ]
endif
;fit2 = poly_fit( radi[int2]-rres, totphaseu[int2], 2 )
zpt2 = -fit2[1]/fit2[2]/2  ; Derivative goes to zero
zpt2x = where( abs(radi-rres-zpt2) eq min(abs(radi-rres-zpt2)) )
_phase0 = poly(zpt2,fit2)  ; = fit2[0] - fit2[1]^2 / 4 / fit2[2] 
fit_phase0 = ( _phase0 + 45 ) mod 360
fit_rres = zpt2 + rres
fit_sigma = k_to_sigma(mm,fit_rres)/( fit2[2]*2*!pi/180 )
sigma_rres = ( pfsig2[1]/fit2[2]/2 )^2 + ( pfsig2[2]*fit2[1]/fit2[2]^2/2 )^2
sigma_rres = sqrt(sigma_rres) * fac
sigma_phase = pfsig2[0]^2 + ( 2*pfsig2[1]*fit2[1]/4/fit2[2] )^2 + $
              ( pfsig2[2]*fit2[1]^2/4/fit2[2]^2 )^2
sigma_phase = sqrt(sigma_phase) * fac
sigma_sigma = ( 4*sigma_rres/fit_rres )^2 + ( pfsig2[2]/fit2[2] )^2
sigma_sigma = k_to_sigma(mm,fit_rres)/(fit2[2]*2*!pi/180) * $
              sqrt(sigma_sigma) * fac
;sigma_rres = 0.01
;sigma_phase = 1.5
;sigma_sigma = 0.3

if not keyword_set(noplot) then begin

  ;oplot, radi-rres, poly( radi-rres, fit1 ), l=3
  ;oplot, radi-rres, (fit2[1]*(radi-rres) + fit2[2]*2*(radi-rres))/180*!dpi, l=4
  dr1 = !x.crange[1] - !x.crange[0]
  xyouts, !x.crange[0] + dr1/30, $
          !y.crange[1] + (!y.crange[1]-!y.crange[0])*9/20, chars=1, '(a)'
  xyouts, !x.crange[0] + dr1/30, $
          !y.crange[1] + (!y.crange[1]-!y.crange[0])/5, chars=1, '(b)'
  xyouts, !x.crange[0] + dr1/30, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/20, chars=1, '(c)'

  if keyword_set(dolzr) then begin
    clzr
    lzr, dir+'soirings_some_wave3'
    th = 8
    clr = 0
  endif else begin
    window, 5
    th = 1
    clr = green()
  endelse
  noxtn = replicate(' ',20)
  !p.multi=0

  ; Height of the plotting window, in characters
  yma = ( float(!d.y_size)/!d.y_ch_size )/!p.charsize
  ; First row will have legends, so add 4 to the usual [4,2] to get 10
  yma1=[(yma-6)*3/4+4,2]  ; First two rows of 8
  yma2=[(yma-6)*5/8+4,(yma-6)/4+2]  ; Third row of 8
  yma3=[(yma-6)*3/8+4,(yma-6)*3/8+2]  ; Fourth and fifth rows of 8
  yma4=[(yma-6)/4+4,(yma-6)*5/8+2]    ; Sixth row of 8
  yma5=[4,(yma-6)*3/4+2]    ; Seventh and eighth rows of 8
  yma12=[(yma-6)/2+4,(yma-6)/4+2]   ; Third and fourth rows of 8
  yma13=[(yma-6)/4+4,(yma-6)/2+2]   ; Fifth and sixth rows of 8
  yma14=[(yma-6)*2/3+4,2]
  yma15=[(yma-6)/3+4,(yma-6)/3+2]
  ; Width of the plotting window, in characters
  xma = ( float(!d.x_size)/!d.x_ch_size )/!p.charsize
  xma1 = [ 13, xma/2 - 3 ]
  xma2 = [ xma/2 + 13, 3 ]
  xma5 = [ 13, 3 ]

  ;plot, /xs, /ys, radi-rres, ridges[*,3] - thwn, xtickn=noxtn, xma=xma5

  if keyword_set(do1e5) then begin
    yr = [-.02,.06]
    yti = .03
  endif else begin
    yr = [-.02,.03]
    yti = .015
  endelse
  plot, /xs, /ys, yr=yr, radi-rres, ridges[*,3] - thwn, xtickn=noxtn, $
        yticki=yti, ytit='Residual!CWavenumber!C(rad/km)', xma=xma5, yma=yma2
  oplot, radi[int1]-rres, ridges[int1,3] - thwn[int1], th=th, color=clr
  oplot, radi-rres, poly( radi-rres, fit1 ) - thwn, l=2
  oplot, [zpt1], poly( zpt1, fit1 ) - thwn[zpt1x], ps=8
  ;oplot, ps=8, radi[round(zeroes)]-rres, (ridges[*,3]-thwn)[round(zeroes)]
  oplot, [0,0], [-1000,1000], l=1
  oplot, [-1000,1000], [0,0], l=1
  xyouts, !x.crange[0] + dr1/30, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/5, chars=1, '(b)'
  xyouts, 2, !y.crange[1] - (!y.crange[1]-!y.crange[0])/5, chars=1, $
        '!Ms!D0!N = '+string( sigma1, fo='(F5.2)' )+pm+ $
        string( sigma_sigma1, fo='(F4.2)' )+'!C'+$
        'r!DL!N = '+string( rres1, fo='(F9.2)' )+pm+$
        string( sigma_rres1, fo='(F4.2)' )

  yrph = [-20,20];-[65,25]
  plot, radi-rres, totphaseu-thphase, /xs, /ys, yr=yrph, xma=xma5, $
        yma=yma4, /noerase, yticki=15, ytit='Residual!CPhase (!Uo!N)', $
        xtickn=noxtn
  oplot, radi[int2]-rres, totphaseu[int2]-thphase[int2], th=th, color=clr
  oplot, radi-rres, poly( radi-rres, fit2 )-thphase, l=2
  oplot, [zpt2], [poly( zpt2, fit2 )]-thphase[zpt2x], ps=8
  oplot, [-1000,1000], [0,0], l=1
  oplot, [0,0], [-1000,1000], l=1
  ;oplot, radi[round(zeroes)]-rres, (totphaseu-thphase)[round(zeroes)], ps=8
  xyouts, !x.crange[0] + dr1/30, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/5, chars=1, '(d)'
  xyouts, 2, !y.crange[1] - (!y.crange[1]-!y.crange[0])/5, chars=1, $
        '!Ms!D0!N = '+string(fit_sigma,fo='(F5.2)')+pm+string( sigma_sigma, $
        fo='(F4.2)' )+'     r!DL!N = '+string( fit_rres, fo='(F9.2)' )+pm+$
        string( sigma_rres, fo='(F4.2)' )+'     !Mf!D0!N = '+$
        string( fit_phase0, fo='(F5.1)' )+pm+ $
        string( sigma_phase, fo='(F3.1)' )
  print, zpt1, zpt2
  print, sigma1, fit_sigma

  plot, radi-rres, totphaseu, /xs, /ys, /noerase, xtickn=noxtn, yticki=3000, $
        ytit='Phase (!Uo!N)', xma=xma5, yma=yma3, xticki=10, yr=[-1000,11500]
  xyouts, !x.crange[0] + dr1/30, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, chars=1, '(c)'
  oplot, radi-rres, thphase, l=1
  oplot, radi-rres, poly( radi-rres, fit2 ), l=2
  oplot, [zpt2], [poly( zpt2, fit2 )], ps=8
  oplot, [0,0], [-10000,10000], l=1
  oplot, radi[int2]-rres, totphaseu[int2], th=th, color=clr

  plot, radi-rres, k_to_sigma(mm,rres) / sigma * (radi-rres), /xs, /ys, $
        /noerase, xtickn=noxtn, ytit='Wavenumber!C(rad/km)', xma=xma5, $
        yma=yma1, xticki=10, yticki=1, l=1
  xyouts, !x.crange[0] + dr1/30, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, chars=1, '(a)'
  oplot, radi-rres, ridges[*,3]
  oplot, radi-rres, poly( radi-rres, fit1 ), l=2
  oplot, [zpt1], [poly( zpt1, fit1 )], ps=8
  oplot, [0,0], [-10000,10000], l=1
  oplot, radi[int1]-rres, ridges[int1,3], th=th, color=clr

endif
xx0 = min(where( radi gt rres ))
radi1 = radi[xx0:n_elements(radi)-1]
common fdensity_wave55, dw55_mm, dw55_phase0, dw55_rres, dw55_sigma, dw55_xi_d
dw55_mm=mm & dw55_phase0=fit_phase0 & dw55_rres=fit_rres & dw55_sigma=fit_sigma
if not keyword_set(noplot) then begin

  pp = [ 0.6d, 14 ]
  function_name='fdensity_wave5a'
  fdensity_wave5a, radi1, pp, vinit
  plot, radi-rres, val, /xs, /ys, yr=[-7,7], xtit=xtit, $
        ytit='!MD!Ms(r) = x(r)', xma=xma5, yma=yma5, /noerase
  oplot, radi1-rres, vinit, th=th, color=clr
  oplot, [0,0], [-10,10], l=1
  xyouts, !x.crange[0] + dr1/30, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, chars=1, '(e)'

  if keyword_set(dolzr) then begin
    clzr
    lzr, dir+'soirings_some_wave4'
    th = 8
    clr = 0
  endif else begin
    window, 2
    th = 1
    clr = green()
  endelse

endif

if keyword_set(plotridge) then begin
  if not keyword_set(intridgepower) then begin
    xx = 50
    ridgepower = wavelet_ridgepower( wave, ridges, nosqrt=nosqrt, /gauss )
    int3 = (where( ridgepower eq max(ridgepower) ))[0]
    int3 = lindgen(2*xx+1) + int3 - xx
    maxamp = interpol( ridges[int3,0], (deriv(smooth(ridgepower,xx)))[int3], 0 )
    int4 = lindgen(n_elements(ridgepower))
  endif else begin
    ; I tried doing it this way, and it was encouraging that the actual 
    ; ridgepower curve is much smoother.  But the location of the maximum
    ; changes depending on which level you use, and it turned out that simply
    ; using the trace of the ridge more accurately matches the input xi_d.
    xx = 1
    ridgepower = wavelet_ridgepower( wave, ridges, nosqrt=nosqrt, $
                                     minpower=10^wavelet_levels[rlv] )
    int4 = where( ridgepower ne 0, count )
    if count eq 0 then stop, int4
    maxamp = interpol( ridges[int4,0], (deriv(ridgepower))[int4], 0 )
  endelse
  ; It looks like real data is too noisy to use this method anyway.
  if not keyword_set(noplot) then begin
    plot, ridges[int4,2]-rres, ridgepower[int4], $
          xtit=xtit, /noerase, yma=yma1, $
          ytit='Height of Wavelet Ridge!Cin Units of !MD!Ms(r)', /xs, xma=xma5
    if not keyword_set(intridgepower) then oplot, ridges[int4,2]-rres, $
          (smooth(ridgepower,xx))[int4], l=2
  endif
endif else begin
  fit_xi_d_only1:
  int4 = where( radi[zeroes] gt rres, count )
  if xi_d eq 4 then int4 = int4[where( radi[zeroes[int4]]-rres lt 50, count )]
  sm = max( zeroes[int4[1:count-1]] - zeroes[int4] )*2
  int4 = ridges[*,0]
  fourier_smwave = 0;1
  if keyword_set(fourier_smwave) then begin
    run_fft, abs(val), power, f, fourier=fourier
    smwave = filter_fft( abs(val), power, f, fourier, $
                          pts=where(1/f lt sm), /noplot )
  endif else begin
    smwave = smooth( abs(val), sm, /edge )
    for j=2,3 do smwave = smooth( smwave, sm, /edge )
  endelse
  ; Select an interval centered on the maximum amplitude, but make sure
  ; that it doesn't contain points where the derivative starts to head
  ; back towards zero
  maxamp1 = (where( smwave eq max(smwave) ))[0]
  mkexed, where( deriv(deriv(smwave)) gt 0 ), exed, z
  int5 = exed[ max(where( exed le maxamp1 )) : $
               min(where( exed ge maxamp1 ))]
  int5 = lindgen(int5[1]-int5[0]+1) + int5[0]
  ; Find the zero-derivative point in the amplitude
  maxamp = interpol( int5, (deriv(smwave))[int5], 0 )
  if not keyword_set(noplot) then begin
    plot, radi[int4]-rres, abs(val[int4]), $
          xtickn=noxtn, /noerase, yma=yma1, $
          ytit='Wave Amplitude', /xs, /ys, yr=[0,7], xma=xma5, yticki=2
    oplot, radi[int4]-rres, smwave[int4], th=th, color=clr
    oplot, [0,0], [-10,10], l=1
  endif
endelse
fit_xi_d = interpol( xiout, lindgen(n_elements(radi)), maxamp )*3^(1./3)
sigma_xi_d = interpol( smooth(deriv(xiout,deriv(xiout,smwave)),sm), $
                       lindgen(n_elements(radi)), maxamp )*3^(1./3)
print, fit_xi_d, xi_d
if not keyword_set(noplot) then begin
  oplot, [1,1]*(radi[maxamp]-rres), [-1e10,max(smwave[int4])], $
         l=2, th=th, color=clr
  xyouts, !x.crange[0] + dr1/30, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, chars=1, '(a)'
  if keyword_set(do1e5) or keyword_set(do13e5) then begin
    fo = '(F6.3)'
  endif else begin
    fo = '(F4.2)'
  endelse
  xyouts, 2, !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, chars=1, $
          '!Mx!DD!N = '+string( fit_xi_d, fo=fo )
endif
if keyword_set(fit_xi_d_only) then goto, fit_xi_d_only2

; Use MPCURVEFIT to find A_L and xi_D
val1 = val[xx0:n_elements(radi)-1]
weights = replicate(1.,n_elements(radi1))
dw55_mm=mm & dw55_phase0=fit_phase0 & dw55_rres=fit_rres & dw55_sigma=fit_sigma
pp = [ 0.6d ]
function_name='fdensity_wave5b'
dw55_xi_d = fit_xi_d
fdensity_wave5b, radi1, pp, vinit
vmodel = mpcurvefit( radi1, val1, weights, pp, $
                     pp_sigma, function_name=function_name, $
                     iter=iter, chisq=chisq, /double, yerror=yerror )

if not keyword_set(noplot) then begin

  plot, radi-rres, val, /xs, /ys, yr=[-7,7], xtickn=noxtn, $
        ytit='!MD!Ms(r) = x(r)', xma=xma5, yma=yma12, /noerase
  oplot, radi1-rres, vinit, th=th, color=clr
  oplot, [0,0], [-10,10], l=1
  xyouts, !x.crange[0] + dr1/30, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, chars=1, '(b)'

  plot, radi-rres, val, /xs, /ys, yr=[-7,7], xtit=xtit, $
        ytit='!MD!Ms(r) = x(r)', xma=xma5, yma=yma13, /noerase
  oplot, radi1-rres, vmodel, th=th, color=clr
  oplot, [0,0], [-10,10], l=1
  xyouts, !x.crange[0] + dr1/30, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, chars=1, '(c)'
  xyouts, 2, !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, chars=1, $
          'A!DL!N = '+string( pp[0], fo='(F6.4)' )

  if keyword_set(dolzr) then begin
    clzr
    lzr, dir+'soirings_some_wave5'
    th = 8
    clr = 0
  endif else begin
    window, 6, xpos=640
    th = 1
    clr = green()
  endelse

  run_fft, totphaseu[int2]-thphase[int2], fft_power, fft_f, $
           x=(radi[int2]-rres)/(radi[1]-radi[0]), /noplot, coeff=fft_coeff
  plot_fft, totphaseu[int2]-thphase[int2], fft_power, fft_f, $
            coeff=fft_coeff, xtit=' ', xtickn=noxtn, $
            ytit='Fourier Power in!CResidual Phase', /no95, /nosignif, $
            /ylog, xma=xma1, yma=yma14
  xyouts, 10^( !x.crange[1] - (!x.crange[1]-!x.crange[0])/20 ), $
          10^( !y.crange[1] - (!y.crange[1]-!y.crange[0])/10 ), chars=1, '(a)', align=1
  _fft_power = fft_power
  for k=1,n_elements(fft_power)-1 do begin
    _fft_power[k] = _fft_power[k-1] + _fft_power[k]
  endfor
  _fft_power = _fft_power / max(_fft_power)
  effscale = interpol( 1/fft_f, _fft_power, 0.9 )
  plot, 1/fft_f, _fft_power, /xlog, xtit='Scale (pixels)', $
        ytit='Fractional Cumulative!CFourier Power', /xs, $
        /ys, /noerase, xma=xma1, yma=yma15
  oplot, [1e-10,effscale,effscale], [0.9,0.9,1e-10], l=1
  xyouts, 10^( !x.crange[1] - (!x.crange[1]-!x.crange[0])/20 ), $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, chars=1, '(b)', align=1
  vv = [ '0.003', '0.03', '0.3', '3', '30', '300', '3000' ]
  axis, xaxis=j, /data, /xs, xtickv=vv, xticks=n_elements(vv)-1, xtickn=vv

  print, 'effscale = '+strtrim(effscale,2)
  save, effscale, filename='effscale.sav'

  fit_xi_d_only2:
  if keyword_set(dolzr) then clzr
  !p.charsize=0
  !p.multi=0

endif

end

