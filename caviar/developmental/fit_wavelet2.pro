; fit_wavelet2.pro
; This version consists mostly of comments and minor updates to fit_wavelet1.
; Real changes will be implemented in fit_wavelet3.pro

; Before running this routine, first do the following:
; usefp = 2  ; To use completely what is in saved file
; restore, 'fitparams.sav'
; print, _fitparams.rrname  ; Then pick your resonance and make the index jfw
; j1 = _fitparams[jfw].j1
; j2 = j1
; .run preparewaveletplots
; .run makewaveletplots

; This will be set.  Use predicted phase rather than fitting for it.
if not keyword_exists(constrainphase) then constrainphase = 1

; usefp=1 causes the selection within the wavelet (yy) to be reused
; usefp=2 further causes the parameters of the fit to be reused
; usefp=-1 makes it clear that neither of these is to be done
; If usefp=0, automatically set usefp=1 iff necessary parameters are present
;yy = 0
if not keyword_set(usefp) then begin
  if not keyword_set(_fitparams) then begin
    usefp = -1
  endif else if not keyword_exists(jfw) then begin
    usefp = -1
  endif else if not keyword_set(*(_fitparams[jfw].yy)) then begin
    usefp = -1
  endif else usefp = 1
endif
if usefp ge 1 then begin
  ; Enter in yy from the saved parameters
  yy = *(_fitparams[jfw].yy)
  nyy = _fitparams[jfw].nyy
endif
if keyword_set(yy) then oplot, yy[*,0], yy[*,1], ps=-4, co=green()
if usefp le 0 then begin
  ; Define a new yy, which selects out the part of the wavelet plot to use
  print, 'Draw lower boundary of wavelet signal to be used.'
  cursor, x, y, 3, /data
  if !mouse.button eq 1 then begin
    print, 'Left-click again to confirm that you are measuring a new wave.'
    cursor, x, y, 3, /data
    if !mouse.button eq 1 then begin
      yy = 0
      rres = 0
    endif
  endif
  while !mouse.button eq 1 do begin
    if keyword_set(yy) then yy = [ yy, [[x],[y]] ] else yy = [[x],[y]]
    nyy = n_elements(yy[*,0])
    yy = yy[ sort(yy[*,0]), * ]
    oplot, yy[*,0], yy[*,1], ps=-4, co=green()
    cursor, x, y, 3, /data
  endwhile
endif

if usefp ge 1 then rrname = _fitparams[jfw].rrname else begin
  ; If there is only one identified resonance within the radial interval, then use it.
  ; Otherwise, ask for user input.
  foo = where( tkm(ring_rads) gt yy[0,0] and tkm(ring_rads) lt yy[nyy-1,0], count )
  if count eq 1 then rrname = ring_rads_legend[foo[0]] else begin
    print, 'Set rrname equal to the resonance label to use'
    print, ring_rads_legend[foo[0]]
    stop
  endelse
endelse
; Extract azimuthal parameter mm from resonance label
colon = strpos( rrname, ':' )
if colon ne -1 then begin
  space = [ rstrpos( rrname, ' ', colon ), $
            strpos( rrname, ' ', colon ) ]
  if space[1] eq -1 then space[1] = strlen(rrname)
  j = float(strmid( rrname, space[0]+1, colon-space[0]-1 ))
  mm = float(strmid( rrname, colon+1, space[1]-colon-1 )) + 1
  satname = strmid( rrname, 0, space[0] )
  psname = 'fit_' + satname + strtrim(fix(j),2) + strtrim(fix(mm-1),2)
endif else stop, 'rrname not recognizable as a resonance.'
; Load kernels and find the expected value of the phase
; makewaveletplots should have already initialized the necessary variables.
kind = 'SPK'
cspice_ktotal, kind, spkcount
if spkcount eq 0 or keyword_set(reloadkernels) then begin
  cspice_furnsh,getenv("CAVIAR_KERNELS")
endif
sat = naifsat( satname )
@get_sat_coords
sat_lon = sat_polar[1]
im_lon = (_keywords[j1].ringplane_aimpoint_longitude)[0]
phase_expected = fix_angles( (im_lon-sat_lon)*mm mod 360, /to360 )

; If we're printing, then set necessary parameters.
; Assume fitting has been done, and avoid redoing calculations.
if keyword_set(dolzr) then begin
  lzr, psname, /port
  plot_color
  @plot_prepare
  cc1 = 35
  cc2 = 247
  rres = _fitparams[jfw].rres
  rres0 = rres
  _phase0 = _fitparams[jfw]._phase0
  phase0 = ( _phase0 + 45 ) mod 360
  sigma = _fitparams[jfw].sigma
  sigma_rres = _fitparams[jfw].sigma_rres
  sigma_phase = _fitparams[jfw].sigma_phase
  sigma_sigma = _fitparams[jfw].sigma_sigma
  fit1 = dblarr(3)
  fit1[2] = 2*!pi/3.08/rres^4*60330.^4*(mm-1)/(sigma*2*!pi/180)
  fit1[0] = fit1[2]*(rres-rres0)^2 + _phase0
  fit1[1] = -2*(rres-rres0)*fit1[2]
  zpt1 = -fit1[1] / fit1[2] / 2
endif else begin
  cc1 = 220
  cc2 = 0
endelse
; Use wavelet_levels (from makewaveletplots.pro) to set colors.
mnp = min(wavelet_levels) & mxp = max(wavelet_levels)
lv = 1./(wavelet_levels[2]-wavelet_levels[1])
wavelet_colors = cc2-findgen((mxp-mnp)*lv+1)/((mxp-mnp)*lv)*(cc2-cc1)

; Translate radii and wavenumbers into array indices.
nx = n_elements(radi)
ny = n_elements(wavenum)
yy1 = [ [interpol( indgen(nx), tkm(radi), yy[*,0] )], $
        [interpol( indgen(ny), wavenum, yy[*,1] )] ]

; Zero out the wavelet for all locations outside the drawn boundary.
wave1 = wave
wave1[0:yy1[0,0],*] = 0
wave1[yy1[nyy-1,0]+1:nx-1,*] = 0
for j=long(yy1[0,0]+1),long(yy1[nyy-1,0]) do begin
  ymin = interpol(yy1[*,1],indgen(nyy),interpol(indgen(nyy),yy1[*,0],j))
  wave1[j,ymin+1:ny-1] = 0
endfor
; ***********************
; Eventually get rid of the square
; ***********************
power1 = abs(wave1^2)  ; This array is for plotting only.
power1[where(power1 eq 0)] = min(abs(wave^2))

; Reconstruct filtered signal from wave1
if param eq 6 then cdelta = 0.776 else stop, 'Need to grapple with cdelta for param ne 6.'
psi0 = !dpi^(-0.25)
vfilt = wavelet_dj * sqrt(wavelet_dt) / (cdelta*psi0) * $
                               ( float(wave1)  # (1./sqrt(wavelet_scale)) )
vrejec = val - vfilt

; Get average wavelet phase from wave1
totphase = unwrap_phase(get_phase( total(wave1,2), wrap=totwrap ))
; z1[*,0] is each the radial location at which totphase passes through 180
; z1[*,1] is the phase value at the location
; z1[*,2] is the value of vfilt at the location
for j=0,( totphase[yy1[nyy-1]-1] - (totphase[yy1[nyy-1]-1] mod 360) )/180 do begin
  z2 = interpol( radi[yy1[0]+1:yy1[nyy-1]-1], $
                 totphase[yy1[0]+1:yy1[nyy-1]-1], j*180 )
  z3 = interpol( vfilt[yy1[0]+1:yy1[nyy-1]-1], $
                 totphase[yy1[0]+1:yy1[nyy-1]-1], j*180 )
  if j eq 0 then z1 = [ [z2], [j*180], [z3] ] $
      else z1 = [ z1, [ [z2], [j*180], [z3] ] ]
endfor
if !d.name eq 'X' then window, 20, ys=1000, xpos=640

; q=0 for the first run-through, in which preliminary calculations are done and
; parameters for fitting are identified
; q=1 is the final run-through
for q=0,1 do begin

if q eq 1 then wset, 20
if keyword_set(dolzr) then begin
  q=1
endif
!y.omargin=[4,2]
oldpm=!p.multi
!p.multi=[0,1,5]
chsz = 2;1.5
xr = [ yy[0,0], yy[nyy-1,0] ]

xtn = replicate(' ',20)
if keyword_set(dolzr) then sh = 5 else sh = 10

; Plot unfiltered input signal
plot, tkm(radi), val, /xs, /ys, xr=xr, xtickn=xtn, ytit='I/F', chars=chsz, $
      yma=[sh,0], tit=rrname

; Plot whole wavelet transform (EDIT: oplot yy)
unget_color
contour, alog10(abs(wave^2)), tkm(radi), wavenum, /xs, /ys, xr=xr, $
    yr=wavelet_yr, /fill, levels=wavelet_levels, $
    c_colors=wavelet_colors, xtickn=xtn, ytit='Wavenumber!C(radians/km)', $
    chars=chsz, yma=[sh,-sh]

; Plot filtered input signal
plot, tkm(radi), vfilt, /xs, /ys, xr=xr, xtickn=xtn, ytit='Filtered I/F', chars=chsz, yma=[2*sh,-sh]
oplot, tkm(z1[*,0]), z1[*,2], ps=3, co=red()

; If fitting parameters have been chosen, then do the fit now and oplot.
; Or, if novm=1, use fit parameters to generate the final model, and oplot
if q eq 1 then begin
  vma = (!y.crange[1]-!y.crange[0])/2
  xx0 = min(where( radi gt rres ))
  radi1 = radi[xx0:n_elements(radi)-1]
  if keyword_set(novm) then begin
    if usefp ge 1 then begin
      pp = _fitparams[jfw].pp
      pp_sigma = _fitparams[jfw].pp_sigma
    endif else begin
      pp = [ vma/10, 10 ]
      pp_sigma = [ 0.0d0, 0 ]
    endelse
    vmodel = fdensity_wave5( radi1, a=pp[0], xi_d=pp[1], mm=mm, phi=phase0, $
                  rres=rres, sigma=sigma, fresnel_integral=fresnel_integral )
    dof = n_elements(radi1)-n_elements(pp)
  endif else begin
    common fdensity_wave55, dw55_mm, dw55_phase0, dw55_rres, dw55_sigma
    dw55_mm=mm & dw55_phase0=phase0 & dw55_rres=rres & dw55_sigma=sigma
    weights = replicate(1.,n_elements(radi1))
    if usefp ge 1 then pp = _fitparams[jfw].pp else begin
      pp = [ vma/10, 10 ]  ; Initial guesses for amplitude and damping
    endelse
    vmodel = mpcurvefit( radi1, vfilt[xx0:n_elements(radi)-1], weights, pp, $
                         pp_sigma, function_name='fdensity_wave5a', $
                         iter=iter, chisq=chisq, /double, yerror=yerror )
    ; Use chi-squared and degrees of freedom to get true uncertainties.
    dof = n_elements(radi1)-n_elements(pp)
    pp_sigma = pp_sigma * sqrt( chisq / dof )
  endelse
  ; ***********************
  ; Should we really be removing the mean from the model?
  ; ***********************
  oplot, tkm(radi1), vmodel-mean(vmodel), co=green()
  ; Run wavelet on the model, and find its wavelet phase to oplot later
  run_wavelet, radi1, vmodel, vwave, /noplot
  vphase = unwrap_phase(get_phase( total(vwave,2), wrap=vwrap) )
endif
if 4 eq 5 then begin
  plot, xr, [-180,180], /nodata, /xs, /ys, yticki=45, xtickn=xtn, $
      ytit='Phase', chars=chsz, yma=[2*sh,-2*sh]
  for j=0,n_elements(totwrap)-2 do oplot, tkm(radi[totwrap[j]:totwrap[j+1]-1]), $
                               fix_angles(totphase[totwrap[j]:totwrap[j+1]-1])
endif

; Plot filtered wavelet transform, with yy
unget_color
contour, alog10(power1), tkm(radi), wavenum, /xs, /ys, xr=xr, $
    yr=wavelet_yr, /fill, nlevels=15, levels=wavelet_levels, $
    c_colors=wavelet_colors, xtickn=xtn, ytit='Wavenumber!C(radians/km)', $
    chars=chsz, yma=[2*sh,-2*sh]
oplot, yy[*,0], yy[*,1], thick=2, co=blue()
; If fitting has been done, then oplot wavemodels and print values
if q eq 1 then begin
  plot_wavemodel, rres, rrname, sigma, color=green()
  if keyword_set(constrainphase) then begin
    ptext = ' (Expected)'
  endif else begin
    ptext = ' +- '+strtrim(sigma_phase,2) + $
                         '  (Expected: '+strtrim(phase_expected,2)+')'
  endelse
  xyouts, !x.crange[0]+(!x.crange[1]-!x.crange[0])/30, chars=1, $
          !y.crange[1]-(!y.crange[1]-!y.crange[0])/8, color=green(), $
          'Sigma = '+strtrim(sigma,2)+' +- '+strtrim(sigma_sigma,2) + '!C' + $
          'Rres  = '+strtrim(rres,2)+' +- '+strtrim(sigma_rres,2) + '!C' + $
          'Phase = '+strtrim(phase0,2) + ptext + '!C' + $
          'A     = '+strtrim(pp[0],2)+' +- '+strtrim(pp_sigma[0],2) + '!C' + $
          'Xi_d  = '+strtrim(pp[1],2)+' +- '+strtrim(pp_sigma[1],2) + '!C' + $
          'Chisq/DOF = '+strtrim(chisq/dof,2)
endif

; Select whether to use all of the accumulated phase, or just the mod 180 points
dots = 1
if keyword_set(dots) then begin
  rr = z1[*,0]
  vv = z1[*,1]
  psdots =  3
endif else begin
  rr = radi
  vv = totphase
  psdots = 0
endelse
; quadraticfit=0 would turn on a piece of code that's no longer used. 
quadraticfit = 1

; Plot the unwrapped phase
plot, tkm(rr), vv, /xs, /ys, xr=xr, ps=psdots, $
      xtit='Radius'+tkmtit(), ytit='Unwrapped Phase', chars=chsz, yma=[0,-2*sh]
!x.omargin=0
!y.omargin=0
!p.multi=oldpm

; Show resonance locations, and fitted rres
solid_diamonds
oplot, tkm(ring_rads), replicate(!y.crange[1],n_elements(ring_rads)), ps=8, co=cyan(), /noclip
xyouts, tkm(ring_rads), replicate(!y.crange[1]-(!y.crange[1]-!y.crange[0])*.025,n_elements(ring_rads)), ring_rads_legend, co=cyan(), align=1, orient=90
if q eq 1 then oplot, [tkm(rres)], [!y.crange[1]], ps=4, co=green(), /noclip

; x0 and x1 are the interval (in data coords) over which to do the quadratic fit
; xdv0 and xdv1 are the same in pixel coords.
; Either use previous values or find new ones, then oplot.
foo = 0
if usefp ge 2 then begin
  x0 = _fitparams[jfw].x0
  x1 = _fitparams[jfw].x1
  xdv0 = _fitparams[jfw].xdv0
  xdv1 = _fitparams[jfw].xdv1
  foo = 1
endif
if not keyword_set(foo) then begin
  if q eq 0 then begin
    cursor, r0, y, 3
    cursor, xdv0, y, 4, /normal
    x0 = min(where( tkm(rr) gt r0 ))
    help, x0
    plots, xdv0*[1,1], [0,!d.y_size], /normal, l=1, color=red()
    cursor, r1, y, 3
    cursor, xdv1, y, 4, /normal
    x1 = max(where( tkm(rr) lt r1 ))
    help, x1
  endif
endif
plots, xdv0*[1,1], [0,!d.y_size], /normal, l=1, color=red()
plots, xdv1*[1,1], [0,!d.y_size], /normal, l=1, color=red()
oplot, tkm(rr[x0:x1]), vv[x0:x1], ps=psdots*4./3, color=red()

if q eq 0 then begin
  if usefp ge 1 then rres0 = _fitparams[jfw].rres else begin
    thoukm = get_thoukm(rr)  ; Poly_fit does not do well with .04% change in x
    foo = where( _ring_rads_legend eq rrname, count )
    if count eq 1 then rres0 = _ring_rads[foo[0]] else rres0 = thoukm
    if keyword_set(rres) then rres0=rres
  endelse
  fit1 = svdfit( rr[x0:x1]-rres0, vv[x0:x1], 3, chisq=chisq, sigma=pfsigma )
  zpt1 = -fit1[1] / fit1[2] / 2
  rres = zpt1 + rres0
  _phase0 = poly(zpt1,fit1)  ; = fit1[0] - fit1[1]^2 / 4 / fit1[2] 
  phase0 = ( _phase0 + 45 ) mod 360
  if keyword_set(constrainphase) then begin
    ; Fitting the phase seems too unconstrained to be useful.  The expected
    ; value is already known, so simply constrain the phase to take this
    ; value, and fit the other parameters.  This requires MPCURVEFIT.
    fit1a = fit1
    dphase = fix_angles( phase_expected - phase0 )
    _phase0 = _phase0 + dphase
    weights = replicate( 1., n_elements(rr[x0:x1]) )
    ppq = rebin(fit1[1:2],2,3)
    _ppq = ppq[*,0]
    ppq_sigma = dblarr(2,3)
    pshift = [ -1, 0, 1 ]
    chisqq = dblarr(3)
    iterqq = 0
    for _j=0,2 do begin
      j = _j
      mpquad:
      fit2 = mpcurvefit( rr[x0:x1]-rres0, vv[x0:x1] - (_phase0+360*pshift[j]), $
                         weights, _ppq, _ppq_sigma, $
                         function_name='quadratic_holdzero', /quiet, $
                         iter=iterq, chisq=_chisqq, /double, yerror=yerrorq )
      ppq[*,j] = _ppq
      ppq_sigma[*,j] = _ppq_sigma
      chisqq[j] = _chisqq
    endfor
    ; Make sure that we are using the value of pshift giving the best chisq.
    jbest = (where( chisqq eq min(chisqq), count ))[0]
    if count ne 1 then stop, 'Tie for jbest!'
    if jbest ne 1 then begin
      if iterqq lt 5 then iterqq = iterqq + 1 else begin
        ; Prevent infinite loop.
        print, 'We''re seeming to have some trouble finding an adequate pshift.'
        stop
      endelse
      if jbest eq 0 then begin
        int1 = [[ 1, 2 ]]
        int2 = [[ 0, 1 ]]
      endif else if jbest eq 2 then begin
        int1 = [[ 0, 1 ]]
        int2 = [[ 1, 2 ]]
      endif else stop  ; This should never happen.
      ppq[*,int1] = ppq[*,int2]
      ppq_sigma[*,int1] = ppq_sigma[*,int2]
      chisqq[int1] = chisqq[int2]
      j = jbest
      ; Note that the line pshift = pshift - 1 + jbest should be included 
      ; here, and is in fit_wavelet3.pro.  As it stands, the algorithm does
      ; neglects to test that we're really at a local minimum in chisqq.
      ; This should have resulted in a tie for jbest, but the chisqq values
      ; differed by very small amounts.
      goto, mpquad
    endif
    ; The best value of pshift is now pshift[jbest] = pshift[1]
    print, 'Pshift = '+strtrim(pshift[jbest],2)+'*360 gives the best chisq, '+$
           'after '+strtrim(iterqq,2)+' iterations.'
    ppq = ppq[*,jbest]
    ppq_sigma = ppq_sigma[*,jbest]
    chisqq = chisqq[jbest]
    dofq = n_elements(rr[x0:x1]) - 2
    _phase0 = _phase0 + 360*pshift[jbest]
    phase0 = ( _phase0 + 45 ) mod 360
    fit1 = [ ppq[0]^2/4/ppq[1] + _phase0, ppq[0], ppq[1] ]
    zpt1 = -fit1[1] / fit1[2] / 2
    rres = zpt1 + rres0
    ; Unlike SVDFIT, MPCURVEFIT does not scale the error bars, so do it now.
    pfsigma = [ 0, ppq_sigma * sqrt( chisqq / dofq ) ]
  endif
  sigma = 2*!pi/3.08/rres^4*60330.^4*(mm-1)/(fit1[2]*2*!pi/180)
  ; When calculating error bars, note that SVDFIT automatically multiplies
  ; the 1-sigma uncertainty by sqrt(chisq/(n-m)), and returns this in the
  ; keyword "sigma" when there are no error bars on the input data values.
  ; Thus, pfsigma are already correct error bars on the fit parameters.
  ; ***********************
  ; These errors should be added in quadrature.  Probably not a big deal, 
  ; if one source of error is dominant.  Fix this in fit_wavelet3.pro.
  ; Also, replace all occurrences of the 3.08 approximation with k_to_sigma.pro
  ; ***********************
  sigma_rres = pfsigma[1] / fit1[2] / 2 + pfsigma[2] * fit1[1] / fit1[2]^2 / 2
  sigma_phase = pfsigma[0] + 2 * pfsigma[1] * fit1[1] / 4 / fit1[2] + $
                pfsigma[2] * fit1[1]^2 / 4 / fit1[2]^2
  sigma_sigma = 2*!pi/3.08/rres^4*60330.^4*(mm-1)/(fit1[2]*2*!pi/180) * $
                ( sigma_rres/rres + pfsigma[2]/fit1[2] )
  if keyword_set(dots) and not keyword_set(quadraticfit) then begin
    ; I wrote the following when I was trying to diagnose a problem with
    ; the phase of the fits.  It breaks down what the quadratic does into
    ; individual steps, but more or less does the same thing.  The problem
    ; turned out not to be with the quadratic, so we should just use that.
    rresg = (ring_rads(where( ring_rads_legend eq rrname, count )))[0]
    if keyword_set(rradjust) then rresg = rresg + rradjust*nmn/2*kminc
    if keyword_set(drr) then begin
      if count ne 1 then stop, 'rresg'
      ndrr = x1-x0+1
      delta_rres = fltarr(ndrr-4)
      for j=0,ndrr-5 do begin
        delta_rres[j] = ( 2*(rr[x0+j+2]-rresg)^2 - (rr[x0+j]-rresg)^2 - $
                                                     (rr[x0+j+4]-rresg)^2 ) / $
                        ( 2*rr[x0+j+2] - rr[x0+j] - rr[x0+j+4] ) / 2
      endfor
    endif else begin
      nmn = 100
      chisq = fltarr(nmn,nmn)
      expon = fltarr(nmn,nmn)
      nmp0 = poly(zpt1,fit1)
      kminc = 0.1
      phinc = 5
      for m=0.,nmn-1 do for n=0.,nmn-1 do begin
        rmn = alog10(rr[x0:x1]-rresg+(m-nmn/2)*kminc)
        vmn = alog10(vv[x0:x1]-nmp0+(n-nmn/2)*phinc)
        fitlin = poly_fit( rmn, vmn, 1, chisq=_chisq )
        chisq[m,n] = _chisq
        expon[m,n] = fitlin[1]
;        window, 1
;        plot, rmn, vmn, ps=4, /xs, /ys
;        oplot, rmn, poly( rmn, fitlin )
;        wait, .1
      endfor
      foo = where( finite(chisq) ne 1, count )
      if count gt 0 then chisq[foo] = 1
      foo = where( finite(expon) ne 1, count )
      if count gt 0 then expon[foo] = 0
      chisqmn = fltarr(nmn)  ; For each value of n (phase), where chisq is min
      expon2 = fltarr(nmn)   ; For same as above, where expon is closest to 2
      for n=0.,nmn-1 do begin
        foo = where( chisq[*,n] eq min(chisq[*,n]), count )
        if count eq 1 then chisqmn[n] = foo
        foo = where( abs(expon[*,n]-2) eq min(abs(expon[*,n]-2)), count )
        if count eq 1 then expon2[n] = foo
      endfor
      chisqmnsm = smooth(chisqmn,10,/edge)
      expon2sm = smooth(expon2,10,/edge)
      f1 = where( chisqmnsm ne 0 and chisqmnsm ne nmn-1 and expon2sm ne 0 and $
                  expon2sm ne nmn-1, count )
      if count eq 0 then stop
      ncross = interpol( (findgen(nmn))[f1], (expon2sm-chisqmnsm)[f1], 0 )
      mcross1 = interpol( chisqmnsm, findgen(nmn), ncross )
      mcross2 = interpol( expon2sm, findgen(nmn), ncross )
      mcross = mean([mcross1,mcross2])
      window, 1
      plot, [0,100], [0,100], /nodata, xtit='n', ytit='m'
      oplot, chisqmn, ps=4, co=red()
      oplot, chisqmnsm, co=red()
      oplot, expon2, ps=4, co=green()
      oplot, expon2sm, co=green()
      oplot, [ncross], [mcross], ps=6, co=cyan()
      rres = rresg - (mcross-nmn/2)*kminc
      _phase0 = nmp0 - (ncross-nmn/2)*phinc
      rmn = alog10(rr[x0:x1]-rres)
      vmn = alog10(vv[x0:x1]-_phase0)
      fitlin = poly_fit( rmn, vmn, 1, chisq=_chisq )
      fitlin0exp = 10.0d0^fitlin[0]
      fit1 = [ fitlin0exp*(rres-rres0)^2 + _phase0, $
               -2*(rres-rres0)*fitlin0exp, fitlin0exp ]
      zpt1 = -fit1[1] / fit1[2] / 2
      phase0 = ( _phase0 + 45 ) mod 360
      sigma = 2*!pi/3.08/rres^4*60330.^4*(mm-1)/(fit1[2]*2*!pi/180)
    endelse
  endif
endif else begin
  oplot, tkm(radi), poly( radi-rres0, fit1 ), co=green(), l=1-sign(psdots)
  oplot, [tkm(rres)], [poly(zpt1,fit1)], ps=4, co=green()
  ;oplot, tkm(radi1), vphase+_phase0-(_phase0 mod 360), co=cyan()
  oplot, tkm(radi1), vphase+round((_phase0-vphase[0])/360)*360, co=cyan()
  xyouts, !x.crange[1]-(!x.crange[1]-!x.crange[0])/30, chars=1, $
          !y.crange[0]+(!y.crange[1]-!y.crange[0])/20, color=green(), $
          'Chisq/DOF = '+strtrim(chisqq/dofq,2), align=1

  ;window, 4
  
endelse
if keyword_set(dolzr) then clzr

endfor

print, 'Sigma = '+strtrim(sigma,2)+' +- '+strtrim(sigma_sigma,2)
print, 'Rres  = '+strtrim(rres,2)+' +- '+strtrim(sigma_rres,2)
print, 'Phase = '+strtrim(phase0,2) + ptext
print, 'A     = '+strtrim(pp[0],2)+' +- '+strtrim(pp_sigma[0],2)
print, 'Xi_d  = '+strtrim(pp[1],2)+' +- '+strtrim(pp_sigma[1],2)
print, 'Chisq/DOF = '+strtrim(chisq/dof,2)

if keyword_set(findfile('fitparams.sav')) then restore, 'fitparams.sav'
fitparams = { j1:j1, rrname:rrname, sat_lon:sat_lon, im_lon:im_lon, mm:mm, $
              yy:ptr_new(yy), nyy:nyy, x0:x0, x1:x1, xdv0:xdv0, xdv1:xdv1, $
              sigma:sigma, rres:rres, _phase0:_phase0, $
              sigma_sigma:sigma_sigma, sigma_rres:sigma_rres, $
              sigma_phase:sigma_phase, pp:pp, pp_sigma:pp_sigma }
if not keyword_set(_fitparams) then _fitparams = fitparams else begin
  jfw = (where( _fitparams.rrname eq rrname, count ))[0]
  if count eq 0 then begin
    _fitparams = [ _fitparams, fitparams ]   ; Add
    jfw = n_elements(_fitparams)-1
  endif
  if count eq 1 then _fitparams[jfw] = fitparams          ; Update
  if count ge 2 then stop, 'multiple structures in _fitparams for '+rrname
endelse
if not keyword_set(nosave) then save, _fitparams, filename='fitparams.sav'

end
