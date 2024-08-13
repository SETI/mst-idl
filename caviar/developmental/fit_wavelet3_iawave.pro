; For this modified version of fit_wavelet3.pro, use 
; $DATA/images/116/EQXSHADOW001/run_fit_wavelet3_iawave.pro

; fit_wavelet3.pro
; Before running this routine, first do the following:
; usefp = 2  ; To use completely what is in saved file
; restore, 'fitparams.sav'
; print, _fitparams.rrname  ; Then pick your resonance and make the index jfw
; j1 = _fitparams[jfw].j1
; j2 = j1
; .run preparewaveletplots
; .run makewaveletplots

if not keyword_exists(jj) then jj = [0,0]
if n_elements(jj) gt 1 then jj = (where(filenames eq image_name))[0]

if keyword_set(reverse) then reverse = 1 else reverse = 0
if keyword_set(forceleft) then forceleft = 1 else forceleft = 0
if not keyword_exists(separate_xi_d) then separate_xi_d = 1
if not keyword_exists(newerrbars) then newerrbars = 1
pm = ' !S+!R!M^ ';' +- '
solid_diamonds
if not keyword_exists(nosqrt) then nosqrt = 1;0

; This will generally be set.  Use predicted phase rather than fitting for it.
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
if keyword_set(yy) and not keyword_set(dolzr) then oplot, yy[*,0], yy[*,1], ps=-4, co=green()
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
    if count gt 0 then print, ring_rads_legend[foo[0]]
    stop
  endelse
endelse
; Don't currently have necessary info for second-order Mimas phase
; or for Iapetus nodal bending wave
if rrname eq 'Mi 5:3' then constrainphase = 0
if rrname eq 'Ia -1:0 BW' then constrainphase = 0
; Furthermore, use the derivative of the phase to plot the wavenumber
; at every location, rather than fitting a single quadratic, for the
; Iapetus nodal bending wave
if rrname eq 'Ia -1:0 BW' then diffphase = 1
; Extract azimuthal parameter mm from resonance label
colon = strpos( rrname, ':' )
if colon ne -1 then begin
  space = [ rstrpos( rrname, ' ', colon ), $
            strpos( rrname, ' ', colon ) ]
  if space[1] eq -1 then space[1] = strlen(rrname)
  j = float(strmid( rrname, space[0]+1, colon-space[0]-1 ))
  mm = float(strmid( rrname, colon+1, space[1]-colon-1 )) + 1
  kk = j - mm
  satname = strmid( rrname, 0, space[0] )
  if keyword_set(_psname) then psname=_psname else begin
    psname = 'fit_' + satname + strtrim(fix(j),2) + strtrim(fix(mm-1),2)
  endelse
endif else stop, 'rrname not recognizable as a resonance.'
; Load kernels and find the expected value of the phase
; makewaveletplots should have already initialized the necessary variables.
kind = 'SPK'
cspice_ktotal, kind, spkcount
if spkcount eq 0 or keyword_set(reloadkernels) then begin
  cspice_furnsh,getenv("CAVIAR_KERNELS")
endif
sat = naifsat( satname )
if not keyword_set(_et) then begin
  if keyword_set(findfile('et.sav')) then restore, 'et.sav'
endif
if not keyword_set(et) then begin
  if keyword_set(_et) then et=_et[jj] else stop, 'No ephemeris time set.'
endif
foo = where( _et eq et, count )
if count gt 0 then begin
  foo1 = where( foo eq jj, count )
  if count eq 0 then stop, 'et is found in _et, but not at the current jj.'
endif
if not keyword_set(polera) then begin
  @get_sat_prepare
endif
@get_sat_coords
sat_lon = sat_polar[1]
if keyword_set(im_lon_scan) then begin
  im_lon = mean(im_lon_scan)
endif else begin
  im_lon = (_keywords[jj].ringplane_aimpoint_longitude)[0]
endelse
if kk eq 0 then begin
  ; First-order ILR
  phase_expected = fix_angles( (im_lon-sat_lon)*mm + reverse*180, /to360 )
endif else begin
  ; Assume higher-order ILR, p=0
  if satname eq 'Pr' then begin
    restore, '/home/borogove/matthewt/idl/iss/jeorbits/jeorbits33c_pp.sav'
    t0q = t0q_pr     ; Time of periapses (days after J2000)
    l0q = l0q_pr     ; Longitude of periapses (degrees)
    rper = rper_pr   ; Radial period (days)
    oper = oper_pr   ; Orbital period (days)
  endif else if satname eq 'Pd' then begin
    restore, '/home/borogove/matthewt/idl/iss/jeorbits/jeorbits33c_pp.sav'
    t0q = t0q_pd
    l0q = l0q_pd
    rper = rper_pd
    oper = oper_pd
  endif else if satname eq 'Ja' then begin
    restore, '/home/borogove/matthewt/idl/iss/jeorbits/jeorbits33c.sav'
    t0q = t0q_j
    l0q = l0q_j
    rper = rper_j
    oper = oper_j
  endif else if satname eq 'Ep' then begin
    restore, '/home/borogove/matthewt/idl/iss/jeorbits/jeorbits33c.sav'
    t0q = t0q_e
    l0q = l0q_e
    rper = rper_e
    oper = oper_e
  endif else begin
    print, 'Unknown satname: '+satname
    if keyword_set(constrainphase) then stop else begin
      print, 'Unable to find expected phase.  Proceeding anyway.'
      phase_expected = 0
      goto, noexpected
    endelse
  endelse
  ; Convert t0q to ephemeris time format (seconds after J2000)
  t0q = t0q * 86400
  ; Find most recent periapse
  foo = max(where( t0q lt et ))
  if foo eq -1 or foo eq n_elements(t0q)-1 then begin
    stop, 'Did not find a single most recent periapse.'
  endif
  phase_expected = fix_angles( $
                       mm*im_lon - (mm+kk)*sat_lon + kk*l0q[foo] + $
                       reverse*180, /to360 )
  noexpected:
endelse

; If we're printing, then set necessary parameters.
; Assume fitting has been done, and avoid redoing calculations.
;if keyword_set(dolzr) then begin
cc1 = 220
cc2 = 0
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
  if (ymin+1) le (ny-1) then wave1[j,ymin+1:ny-1] = 0
endfor

; Create wavelet modulus/power arrays for plotting
power = abs(wave)
if keyword_set(soirings_plot) then power1 = abs(wave) else power1 = abs(wave1)
if keyword_set(nosqrt) then power = power^2
if keyword_set(nosqrt) then power1 = power1^2
foo = where( power1 eq 0, count )
if count gt 0 then power1[foo] = min(power)

; Reconstruct filtered signal from wave1
if not keyword_set(param) then cdelta = 0.776 $
	else if param eq 6 then cdelta = 0.776 $
	else cdelta = 0.776 * (double(param)/6)^(-1.024)
psi0 = !dpi^(-0.25)
vfilt = wavelet_dj * sqrt(wavelet_dt) / (cdelta*psi0) * $
                               ( float(wave1)  # (1./sqrt(wavelet_scale)) )
vrejec = val - vfilt

; Get average wavelet phase from wave1
phase = get_phase( total(wave1,2), wrap=totwrap )
totphase = unwrap_phase(phase)
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
if not keyword_set(dolzr) then window, 20, ys=1000, xpos=640

if keyword_set(diffphase) then begin
  rr = radi
  vv = totphase
  foo = where( _ring_rads_legend eq rrname, count )
  if count eq 1 then rres0 = _ring_rads[foo[0]] else stop
  rres = rres0
  phase0 = 0
  wavek = deriv( radi, vv )*!dpi/180
  sigma = k_to_sigma(mm,rres) * (radi-rres) / wavek
  x0 = min(where( finite(wavek) ))
  x1 = max(where( finite(wavek) ))
  xdv0 = radi[x0]
  xdv1 = radi[x1]
  ; fit1 = svdfit( rr[x0:x1]-rres0, vv[x0:x1], 3, chisq=chisq, sigma=pfsigma )
  ianbwr = [ 121300.0d0, 122357, 123250, 124000, 124890 ]
  ianbwpx = [ min(where( radi gt ianbwr[0] )), $
              max(where( radi lt ianbwr[1] )), $
              min(where( radi gt ianbwr[1] )), $
              max(where( radi lt ianbwr[2] )), $
              min(where( radi gt ianbwr[2] )), $
              max(where( radi lt ianbwr[3] )), $
              min(where( radi gt ianbwr[3] )), $
              max(where( radi lt ianbwr[4] )) ]
  sigmafit = sigma*0
  foo = lindgen(ianbwpx[1]-ianbwpx[0]+1) + ianbwpx[0]
  sigmafit[foo] = poly( radi[foo]-rres, svdfit(radi[foo]-rres,sigma[foo],2) )
  foo = lindgen(ianbwpx[3]-ianbwpx[2]+1) + ianbwpx[2]
  sigmafit[foo] = poly( radi[foo]-rres, svdfit(radi[foo]-rres,sigma[foo],2) )
  ;foo = lindgen(ianbwpx[5]-ianbwpx[4]+1) + ianbwpx[4]
  ;sigmafit[sfoo[foo]] = (smooth(sigma[sfoo],100,/edge_t))[foo-x0]
  sfoo = where( finite(sigma) and sigma lt 50 and sigma gt 10 )
  foo = where( sfoo ge ianbwpx[4] and sfoo le ianbwpx[5] )
  sigmafit[sfoo[foo]] = (smooth(sigma[sfoo],100,/edge_t))[foo]
  foobad = where( finite(sigmafit[ianbwpx[4]:ianbwpx[5]]) eq 0 or $
                  sigmafit[ianbwpx[4]:ianbwpx[5]] le 10 or $
                  sigmafit[ianbwpx[4]:ianbwpx[5]] ge 50, count ) + ianbwpx[4]
  for j=0,count-1 do begin
    ngood = 5
    foogood = indgen(ngood) - (ngood-1)/2 + foobad[j]
    for k=ngood-1,0,-1 do begin
      test = where( foogood[k] eq foobad, ntest )
      if ntest eq 1 then foogood = vec_remove( foogood, test )
    endfor 
    sigmafit[foobad[j]] = mean(sigmafit[foogood])
;    stop
  endfor
  foo = lindgen(ianbwpx[7]-ianbwpx[6]+1) + ianbwpx[6]
  sigmafit[foo] = poly( radi[foo]-rres, svdfit(radi[foo]-rres,sigma[foo],2) )
  wavekfit = k_to_sigma(mm,rres) * (radi-rres) / sigmafit
  if not keyword_set(noplot) then begin
    if keyword_set(dolzr) then begin
      psname = '116eqxshadow001_iawave'
      lzr, psname+'_wavelet', aspect=0.4
      plot_color
      @plot_prepare
      specify_levels = 1
    endif else begin
      wset, 2
      specify_levels = 0
    endelse 
    hang = 1
    minlam = 18
    mooncolor = 0
    wavelet_wavelength = [ '20', '30', '50', '100', '300' ]
    wavelet_plotyti = 0.0001
    ring_rads = 122052.5 & ring_rads_legend = 'A Ring Inner Edge'
    ring_rads = [ ring_rads, resloc(1,0,608,res_descrip=res_descrip,/short) ]
    ring_rads_legend = [ ring_rads_legend, res_descrip ]
    ring_rads = [ ring_rads, resloc(-1,0,608,res_descrip=res_descrip,bending=1,/short) ]
    ring_rads_legend = [ ring_rads_legend, res_descrip ]
    ring_rads = [ ring_rads, resloc(7,5,611,res_descrip=res_descrip,/short,je=2) ]
    ring_rads_legend = [ ring_rads_legend, 'Ja/'+res_descrip ]
    ring_rads = [ ring_rads, resloc(6,5,615,res_descrip=res_descrip,/short) ]
    ring_rads_legend = [ ring_rads_legend, '!C'+res_descrip ]
    ring_rads = [ ring_rads, resloc(7,6,615,res_descrip=res_descrip,/short) ]
    ring_rads_legend = [ ring_rads_legend, res_descrip ]
    ring_rads = [ ring_rads, resloc(5,4,617,res_descrip=res_descrip,/short) ]
    ring_rads_legend = [ ring_rads_legend, res_descrip ]
    ring_rads = [ ring_rads, resloc(6,5,616,res_descrip=res_descrip,/short) ]
    ring_rads_legend = [ ring_rads_legend, res_descrip ]
    ring_rads = [ ring_rads, resloc(7,6,615,res_descrip=res_descrip,/short) ]
    ring_rads_legend = [ ring_rads_legend, res_descrip ]
    ring_rads = [ ring_rads, resloc(8,6,611,res_descrip=res_descrip,bending=1,/short,je=2) ]
    ring_rads_legend = [ ring_rads_legend, 'Ja/'+res_descrip ]
    ;ring_rads = [ ring_rads, resloc(4,3,611,res_descrip=res_descrip,/short) ]
    ;ring_rads_legend = [ ring_rads_legend, 'Ja/'+res_descrip ]
    ;ring_rads = [ ring_rads, resloc(6,5,617,res_descrip=res_descrip,/short) ]
    ;ring_rads_legend = [ ring_rads_legend, res_descrip ]
    ;ring_rads = [ ring_rads, resloc(7,6,616,res_descrip=res_descrip,/short) ]
    ;ring_rads_legend = [ ring_rads_legend, res_descrip ]
    wavelet_xr = [120.8,125.2]
    @run_wavelet1
    oplot, tkm(ring_rads[[0,0]]), !y.crange, l=2
    oplot, tkm(radi), wavek, ps=3
    oplot, tkm(radi), wavekfit
    if keyword_set(dolzr) then begin
      clzr
      lzr, psname+'_waveletphase'
      !p.charsize = 3
    endif else begin
      wset, 20
    endelse
    !p.multi = [0,1,6]
    !y.omargin = [4,2]
    !y.margin = 0
    notn = replicate(' ',20)
    plot_nosci, tkm(radi), vfilt, xr=wavelet_xr, /xs, /ys, xtickn=notn, ytit='I/F', yticki=.00005
    xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.025, $
            !y.crange[1] - (!y.crange[1]-!y.crange[0])*.2, '(a)', chars=1.5
    plot, wavelet_xr, [-180,180], /xs, /ys, /nodata, xtickn=notn, $
          ytit='Phase (!Uo!N)', yticki=90
    for j=0,n_elements(totwrap)-2 do oplot, $
                             tkm(radi[totwrap[j]:totwrap[j+1]-1]), $
                             phase[totwrap[j]:totwrap[j+1]-1]
    xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.025, $
            !y.crange[1] - (!y.crange[1]-!y.crange[0])*.2, '(b)', chars=1.5
    plot_nosci, tkm(radi[x0:x1]), totphase[x0:x1], xr=wavelet_xr, /xs, /ys, $
                xtit='Radius'+tkmtit(), ytit='Unwrapped!CPhase (!Uo!N)', yticki=10000
    xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.025, $
            !y.crange[1] - (!y.crange[1]-!y.crange[0])*.2, '(c)', chars=1.5
    !y.omargin = 0
    !y.margin = [4,2]
    if keyword_set(dolzr) then begin
      clzr
      !p.charsize = 1.5
      lzr, psname+'_protosigmamodel'
    endif else stop
    !p.multi = [0,2,2]
    plot, tkm(radi[x0:x1]), sigma[x0:x1], ps=3, yr=[0,50], /xs, /ys, $
          xtit='Radius'+tkmtit(), ytit='Model surface density !Ms (g/cm!U2!N)'
    sfoo = where( finite(sigma) and sigma lt 50 and sigma gt 0 )
    oplot, tkm(radi[sfoo]), smooth(sigma[sfoo],100,/edge_t), l=2
    foo = lindgen(ianbwpx[7]-ianbwpx[0]+1) + ianbwpx[0]
    oplot, tkm(radi[foo]), sigmafit[foo]
    if keyword_set(dolzr) then begin
      clzr
      lzr, psname+'_sigmamodel'
      !p.multi = [0,2,2]
    endif
    j75 = (ring_rads[where( ring_rads_legend eq 'Ja/Ep 7:5' )])[0]
    a65 = (ring_rads[where( ring_rads_legend eq '!CAt 6:5' )])[0]
    p54 = (ring_rads[where( ring_rads_legend eq 'Pd 5:4' )])[0]
    pr65 = (ring_rads[where( ring_rads_legend eq 'Pr 6:5' )])[0]
    a76 = (ring_rads[where( ring_rads_legend eq 'At 7:6' )])[0]
    ;pan109 = (ring_rads[where( ring_rads_legend eq 'Pan 10:9' )])[0]
    aredge = (ring_rads[where( ring_rads_legend eq 'A Ring Inner Edge' )])[0]
    foo = where( sigmafit gt 0 )
    plot, tkm(radi[foo]), sigmafit[foo], xr=wavelet_xr, xtit='Radius'+tkmtit(), $
          ytit='Model surface density !Ms (g/cm!U2!N)', /xs
    solid_squares
    oplot, [tkm(j75),123], [11.5,13], ps=8
    xyouts, 123.15, 13-.8, chars=1, 'Tiscareno et al. (2006)';, ApJL)'
    solid_circles
    oplot, [tkm(a76),123], [32.6,10], ps=8
    ;oplot, [tkm(pan109)], [37.7], ps=8
    xyouts, 123.15, 10-.8, chars=1, 'Tiscareno et al. (2007)';, Icarus)'
    solid_triangles
    oplot, [tkm(a65),123], [15.4,7], ps=8
    xyouts, 123.15, 7-.8, chars=1, 'Colwell et al. (2009)';, Icarus)'
    solid_stars
    oplot, [tkm(p54),123], [28,4], ps=8
    oplot, [tkm(pr65)], [38], ps=8
    xyouts, 123.15, 4-.8, chars=1, 'This work'
    oplot, [1,1]*tkm(aredge), !y.crange, l=1
    xyouts, tkm(aredge)-.05, 39, 'Cass. Div.!CA Ring  ', chars=1, orient=90, align=1
    xyouts, tkm(j75), 11.5-2.5, chars=1, 'J/E 7:5'
    xyouts, tkm(a65), 15.4-2.5, chars=1, 'Atlas 6:5'
    xyouts, tkm(p54)-.05, 28-2.5, chars=1, /align, 'Pandora 5:4'
    xyouts, tkm(pr65)+.05, 38, chars=1, 'Prometheus 6:5'
    xyouts, tkm(a76), 32.6-2.5, chars=1, 'Atlas 7:6'
    ;xyouts, tkm(pan109), 37.7+1, chars=1, /align, 'Pan 10:9'
    xyouts, 121.45, 12, 'Iapetus -1:0', chars=1, orient=31
    xyouts, 124, 33, 'Iapetus -1:0', chars=1, orient=11
    if keyword_set(dolzr) then begin
      clzr
      lzr, psname+'_mecfig'
      !p.multi = [0,2,2]
    endif
    restore, '$DATA/images/vims/089/GAMCRU/VIMS_2008_290_GAMCRU_I_TAU_10KM.sav'
    plot, [117.5,129.5], [-.015,.13], /xs, /ys, /nodata, xtit='Radius'+tkmtit(), $
          ytit='Mass extinction!Ccoefficient !Mk (cm!U2!N/g)'
    oplot, [120.9,120.9], !y.crange, l=1
    oplot, [122.05,122.05], !y.crange, l=1
    xyouts, mean([117.5,120.9]), 0, 'Cass. Div.!C(Main Part)', align=.5, chars=1
    xyouts, mean([120.9,122.05]), 0, 'C.D.!C(Ramp)', align=.5, chars=1
    xyouts, 123.5, -.005, 'A Ring', chars=1
    solid_triangles
    c09rad = [ resloc(9,7,616), resloc(6,5,618), $
               ;resloc(5,4,615), resloc(7,6,618), $
               resloc(9,7,617), resloc(6,5,615) ]
    ;c09kappa = [ .111, .083, $
    ;             ;.068, .086, $
    ;             .118, .033 ]
    c09sigma = [ 1.10, 0.98, $
                 ;1.31, 3.51, $
                 5.76, 15.4 ]
    c09tau = c09sigma*0
    for j=0,n_elements(c09rad)-1 do begin
      foo = min(where( radius ge c09rad[j] ))
      c09tau[j] = mean(tau[foo:foo+3])
    endfor
    c09kappa = c09tau / c09sigma
    ; ==> tau = 0.122, 0.0813, 0.0891, 0.680, 0.302, 0.508
    oplot, [tkm(c09rad),123], [c09kappa,.10], ps=8
    xyouts, 123.5, .10-.003, chars=1, 'Colwell et al. (2009)';, Icarus)'
    solid_squares
    t06rad = [ resloc(7,5,611,je=2), resloc(9,7,611,je=2) ]
    t06sigma = [ 11.5, 43.0 ]
    t06tau = t06sigma*0
    for j=0,n_elements(t06rad)-1 do begin
      foo = min(where( radius ge t06rad[j] ))
      t06tau[j] = mean(tau[foo:foo+3])
    endfor
    t06kappa = t06tau / t06sigma
    oplot, [tkm(t06rad),123], [t06kappa,.12], ps=8
    xyouts, 123.5, .12-.003, chars=1, 'Tiscareno et al. (2006)';, ApJL)'
    solid_circles
    t07rad = [ resloc(5,4,615), resloc(7,6,618), resloc(7,6,615), resloc(10,9,618), resloc(8,7,615), $
               resloc(12,11,618), resloc(13,12,618), resloc(15,13,616), resloc(13,11,617), resloc(9,8,615), $
               resloc(8,7,616), resloc(10,9,615), resloc(19,18,618), resloc(9,8,616), resloc(22,21,618), $
               resloc(23,22,618), resloc(8,7,617), resloc(24,23,618), resloc(27,26,618) ]
    t07sigma = [ 1.52, 3.32, 32.61, 37.65, 35.51, 36.7, 37.51, 38.28, 37.45, 37.88, 49.08, 39.93, 39.43, 46.53, $
                 40.87, 41.52, 41.67, 47.93, 43.49 ]
    t07tau = t07sigma*0
    for j=0,n_elements(t07rad)-1 do begin
      foo = min(where( radius ge t07rad[j] ))
      t07tau[j] = mean(tau[foo:foo+3])
    endfor
    t07kappa = t07tau / t07sigma
    oplot, [tkm(t07rad),123], [t07kappa,.11], ps=8
    xyouts, 123.5, .11-.003, chars=1, 'Tiscareno et al. (2007)';, Icarus)'
    solid_stars
    twrad = [ resloc(5,4,617), resloc(6,5,616) ]
    twsigma = [ 28., 37 ]
    twtau = twsigma*0
    for j=0,n_elements(twrad)-1 do begin
      foo = min(where( radius ge twrad[j] ))
      twtau[j] = mean(tau[foo:foo+3])
    endfor
    twkappa = twtau / twsigma
    oplot, [tkm(twrad),123], [twkappa,.09], ps=8
    oplot, [123.5,124.2], [.09,.09]
    xyouts, 124.5, .09-.003, chars=1, 'This work'
    taufit = sigmafit*0
    bounds = [ 120000.0d0, 122050, 122345, 126500 ]
    for j=0,n_elements(bounds)-2 do begin
      foo1 = where( radius gt bounds[j] and radius lt bounds[j+1] )
      bounds[2] = 122357
      foo2 = where( radi gt bounds[j] and radi lt bounds[j+1] )
      bounds[2] = 122345
      taufit[foo2] = interpol( smooth(tau[foo1],10,/edge_t), $
                               radius[foo1], radi[foo2] )
    endfor
    foo = where( sigmafit gt 0 )
    oplot, tkm(radi[foo]), taufit[foo]/sigmafit[foo]
    if keyword_set(dolzr) then clzr
  endif
  stop
endif

; Enter fitted values into fitparams, and save it (unless nosave=1).
if not keyword_set(fitparams_savefile) then fitparams_savefile='fitparams.sav'
_fitparams = fitparams
_fitparams = { jj:jj, rrname:rrname, sat_lon:sat_lon, im_lon:im_lon, mm:mm, $
              yy:ptr_new(yy), nyy:nyy, x0:x0, x1:x1, xdv0:xdv0, xdv1:xdv1, $
              sigma:sigma, rres:rres }
if not keyword_set(nosave) then save, _fitparams, filename=fitparams_savefile

end

