; To create the Figure soirings_some_wave_analyze_errors:
; constrainphase=1
; donoise=3
; dolzr=1
; bothplots=1
; .run soirings_some_wave_analyze_errors
; donoise=0
; .run soirings_some_wave_analyze_errors

; Restore the results of run_soirings_some_wave.pro
if not keyword_set(constrainphase) then restore, '/home/borogove/matthewt/idl/iss/soirings/run_soirings_some_wave.sav' else begin &$
  restore, '/home/borogove/matthewt/idl/iss/soirings/run_soirings_some_wave_cphase1.sav' &$
  _inparams = inparams &$
  _outparams = outparams &$
  restore, '/home/borogove/matthewt/idl/iss/soirings/run_soirings_some_wave_cphase2.sav' &$
  inparams = [ _inparams, inparams ] &$
  outparams = [ _outparams, outparams ] &$
endelse
if keyword_set(donoise) then if donoise eq 3 then begin &$
  _inparams = inparams[where( inparams[*,4] ne 0 ),*] &$
  _outparams = outparams[where( inparams[*,4] ne 0 ),*,*] &$
  restore, '/home/borogove/matthewt/idl/iss/soirings/run_soirings_some_wave_cphase3a.sav' &$
  _inparams = [ _inparams, inparams ] &$
  _outparams = [ _outparams, outparams ] &$
  restore, '/home/borogove/matthewt/idl/iss/soirings/run_soirings_some_wave_cphase3.sav' &$
  inparams = [ _inparams, inparams ] &$
  outparams = [ _outparams, outparams ] &$
  outparams = outparams[bsort( inparams[*,4] ),*,*]
  inparams = inparams[bsort( inparams[*,4] ),*]
  outparams = outparams[bsort( inparams[*,3] ),*,*]
  inparams = inparams[bsort( inparams[*,3] ),*]
  outparams = outparams[bsort( inparams[*,1] ),*,*]
  inparams = inparams[bsort( inparams[*,1] ),*]
endif

!p.charsize=1.5
;!x.omargin=[10,3]
!y.omargin=[4,2]
;!x.margin=0
!y.margin=0
solid_diamonds
notn = replicate(' ',20)
; Assume that inparams has 5 variables to plot, 6 values of noise, 
; an array of phases, and no other variations.
phases = inparams[*,3]
phase = phases[uniq( phases, sort(phases) )]
noises = inparams[*,4]
noise = noises[uniq( noises, sort(noises) )]

if keyword_set(donoise) then begin
  if donoise eq 3 then begin
    nn = n_elements(inparams[*,1])
    nxi = 5;3
    xiind = indgen(nxi)
    ;inparams = reform( inparams[*,1], nn, 1 )
    ;outparams = outparams[*,1,*]
    inparams = reform( inparams[*,1], nn/nxi, nxi )
    outparams = reform( outparams[*,1,*], nn/nxi, nxi, 2 )
    _inparams = inparams
    _outparams = outparams
    _outparams[*,*,1] = 0
;    phases = rebin( phases, nn/nxi )
    phases = phases[0:nn/nxi-1]
    noises = noises[0:nn/nxi-1]
    ;varname = 'Xi_D = '+[ '4', '10', '20' ]
    varname = '!Mx!DD!N = '+[ '4', '7', '10', '15', '20' ]
  endif else goto, notdonoise3
endif else begin
  notdonoise3:
  xiind = 1
  ; Outparams[*,[0,4]] contains amplitude and rres, 
  ;while inparams[*,[0,4]] has mm and noise
  _inparams = inparams
  _inparams[*,0] = 1      ; Input amplitude
  _inparams[*,4] = 1.3e5          ; Input rres
  ; Outparams[*,0:1,0] contains ampltiude and xi_D, while 
  ; outparams[*,0:1,1] contains the stddev of the noise added to the signal, 
  ; and the second derivative of the fitted peak for xi_D.
  _outparams = outparams
  _outparams[*,1,1] = 0.149 * _outparams[*,0,1]^0.6
  _outparams[*,0,1] = 0
  _outparams[*,[2,4],1] = _outparams[*,[2,4],1]; * 3.8
  varname = [ 'Amplitude', 'Xi_D', 'Sigma', 'Phase', 'Rres' ]
endelse

nvar = (size(inparams))[2];5
nnoise = n_elements(noise);6
if !d.name eq 'X' then !p.multi=[0,nnoise,nvar]  
_deviat = fltarr( nnoise, nvar )

if keyword_set(constrainphase) then xs=1920 else xs=1024
if !d.name eq 'X' then window, xs=xs, ys=1024, /free
for j=0,nvar-1 do for i=0,nnoise-1 do begin
  ;if i eq 0 then ytn='' else ytn=notn
  ;if i eq 0 then ytit='Residual!C'+varname[j] else ytit=''
  ytn=''
  ytit='Residual!C'+varname[j]
  if j eq 0 then tit='Noise = '+string(noise[i],fo='(F4.2)') else tit=''
  if j eq nvar-1 then xtn='' else xtn=notn
  if j eq nvar-1 then xtit='Phase' else xtit=''
  int = where( noises eq noise[i] )
  deviat = outparams[int,j,0] - _inparams[int,j]
  _deviat[i,j] = stddev(deviat)
  if not keyword_set(dolzr) and not keyword_set(bothplots) then begin
    plot, phases[int], deviat, ps=8, xticki=90, tit=tit, $
          xtit=xtit, ytit=ytit, xtickn=xtn, ytickn=ytn, xr=[-30,360], /xs
    oploterr, phases[int], deviat, _outparams[int,j,1], 4
    oplot, !x.crange, [0,0], l=1
    xyouts, !x.crange[1] - (!x.crange[1]-!x.crange[0])/10, chars=1.5, al=1, $
            !y.crange[1] - (!y.crange[1]-!y.crange[0])/10, _deviat[i,j], $
            co=ctred()
    ratio = _deviat[i,j] / mean(_outparams[int,j,1])
    xyouts, !x.crange[1] - (!x.crange[1]-!x.crange[0])/10, chars=1.5, al=1, $
            !y.crange[1] - (!y.crange[1]-!y.crange[0])/5, ratio, co=ctgreen()
  endif
endfor

!y.omargin=0
!y.margin=[4,2]
if 4 eq 5 then begin
if !d.name eq 'X' then window, /free
!p.multi=[0,2,3]
if keyword_set(oneplot) then begin
  !p.multi=0
  plot, [.01,10], [.001,.1], /nodata, /xlog, /ylog, $
        xtit='Noise/Amplitude Ratio', $
        ytit='Standard Deviation of!CFitted Values of !Mx!DD!N'
  clr = [ ctcyan(), ctgreen(), ctyellow(), ctred(), ctblue(), ctpurple() ]
endif
for j=0,n_elements(xiind)-1 do begin
  xi_d = inparams[0,j]
  int = indgen(nnoise)
  if xi_d eq 7 then int = int[1:nnoise-1]
  deviat = _deviat[int,xiind[j]]; / xi_d
  if keyword_set(oneplot) then oplot, noise[int], deviat, ps=8, co=clr[j] else $
  plot, noise[int], deviat, ps=8, /ylog, /xlog, $
        xtit='Noise/Amplitude Ratio', $
        ytit='Standard Deviation of!CFitted Values of !Mx!DD!N'
  fit = svdfit( alog10(noise[int]), $
                alog10(deviat), 2, chisq=chisq, sigma=pfsigma )
  ;oplot, noise[int], 10^fit[0] * noise[int]^fit[1]
  oplot, 10^!x.crange, 10^(poly( !x.crange, fit ))
  xyouts, 10^(!x.crange[0] + (!x.crange[1]-!x.crange[0])/10), $
          10^(!y.crange[1] - (!y.crange[1]-!y.crange[0])/10), $
          '(!Ms!D!Mx!N/!Mx!DD!N) = '+string(10^fit[0],fo='(F5.3)')+' N!U'+$
          string(fit[1],fo='(F4.2)')
  print, '( '+strtrim( 10^fit[0], 2 )+' +- '+$
         strtrim( alog(10)*10^fit[0]*pfsigma[0], 2 )+$
         ' ) * N^( '+strtrim( fit[1], 2 )+' +- '+strtrim(pfsigma[1], 2 )+' )'
  ;print, '( '+strtrim( fit[0], 2 )+' +- '+strtrim( pfsigma[0], 2 )+$
  ;       ' ) + N*( '+strtrim( fit[1], 2 )+' +- '+strtrim(pfsigma[1], 2 )+' )'
endfor
endif

if keyword_set(dolzr) then begin
  lzr, 'soirings_some_wave_analyze_errors'
  @plot_prepare
endif else if not keyword_set(bothplots) then window, 1
if not (keyword_set(bothplots) and not keyword_set(dolzr)) then !p.multi=[0,2,2]
if keyword_set(donoise) then if donoise eq 3 then begin
  xi_d = rebin( inparams[0,*], nnoise, nvar )
  deviat = _deviat / xi_d
  ; All of xi_D=4 is bad data, along with the single point xi_D=7 and noise=.05
  deviat = (reform( deviat, nnoise*nvar ))[nnoise+1:nnoise*nvar-1]
  _noise = noise
  for j=1,nvar-1 do _noise = [ _noise, noise ]
  _noise = _noise[nnoise+1:nnoise*nvar-1]
  xi_d = xi_d[nnoise+1:nnoise*nvar-1]
  plot, _noise, deviat, /nodata, /ylog, /xlog, yr=[.0008,.1], /ys, $
        xtit='Noise/Amplitude Ratio', $
        ytit='!Ms!D!Mx!N/!Mx!DD!N'
  ps = [ 0, 4, 8, 6, 8 ]
  dx = !x.crange[1]-!x.crange[0]
  dy = !y.crange[1]-!y.crange[0]
  for j=1,nvar-1 do begin
    if j eq nvar-1 then solid_circles
    int = where( xi_d eq inparams[0,j] )
    oplot, _noise[int], deviat[int], ps=ps[j]
    oplot, [10^(!x.crange[0] + dx/10)], $
           [10^(!y.crange[1] - dy/10*j + dy/30)], ps=ps[j]
    xyouts, [10^(!x.crange[0] + dx/10)], $
            [10^(!y.crange[1] - dy/10*j)], '   '+varname[j]
  endfor
  fit = svdfit( alog10(_noise), $
                alog10(deviat), 2, sigma=pfsigma )
  oplot, 10^!x.crange, 10^(poly( !x.crange, fit ))
  ;xyouts, 10^(!x.crange[0] + dx/10), $
  ;        10^(!y.crange[1] - dy/10), $
  ;        '(!Ms!D!Mx!N/!Mx!DD!N) = '+string(10^fit[0],fo='(F5.3)')+' N!U'+$
  ;        string(fit[1],fo='(F4.2)')
  print, '( '+strtrim( 10^fit[0], 2 )+' +- '+$
         strtrim( alog(10)*10^fit[0]*pfsigma[0], 2 )+$
         ' ) * N^( '+strtrim( fit[1], 2 )+' +- '+strtrim(pfsigma[1], 2 )+' )'
  if keyword_set(bothplots) then dolzr=0
endif
if not keyword_set(donoise) then begin
  deviat = _deviat[1:nnoise-1,0]
  _noise = noise[1:nnoise-1]
  plot, _noise, deviat, ps=8, xtit='Noise/Amplitude Ratio', ytit='!Ms!DA!N/A!DL!N', $
        xr=[0,2.2], yr=[0,0.055], /xs, /ys
  fit = svdfit( _noise, deviat, 2, sigma=pfsigma )
  oplot, !x.crange, poly( !x.crange, fit )
  print, '( '+strtrim( fit[0], 2 )+' +- '+$
         strtrim( pfsigma[0], 2 )+$
         ' ) + N*( '+strtrim( fit[1], 2 )+' +- '+strtrim(pfsigma[1], 2 )+' )'
  if keyword_set(bothplots) and keyword_exists(dolzr) then dolzr=1
endif

if keyword_set(dolzr) then clzr

end
