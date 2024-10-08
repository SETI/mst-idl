pro jemodel, rl1, rl2, rr, dw, nr=nr, jin=jin, time=time, $
             imlon=imlon, im_lon_scan=im_lon_scan, $
             image_mid_time=image_mid_time, nowrite=nowrite, $
             sigma=sigma, xi_d=xi_d, separate=separate, tit=_tit, yr=yr, $
             phasej=phasej, phasee=phasee, radi=radi, val=val, $
             reverse=reverse, overplot=overplot, noplot=noplot, $
             decay=decay, zerophase=zerophase, firstonly=firstonly, $
             printright=printright, dw2=dw2, usegreen=usegreen, $
             nolegend=nolegend, onebox=onebox, resample=resample, $
             xticki=xticki, rres=rres, dradi=dradi, pmulti=pmulti, $
             noxaxis=noxaxis, noyaxis=noyaxis

if n_params() eq 0 then begin
  print, 'Syntax:  JEMODEL, rl1, rl2, rr, dw, time=, image_mid_time=, jin='
  print, 'Makes a model of the rl1:rl2 density wave excited by Janus and Epimetheus.'
  print, 'Outputs rr and dw are the radius and optical depth of the density wave.'
  print, 'jin=1 indicates that Janus is currently interior to Epimetheus, jin=0 the opposite (default).'
  print, 'time indicates the amount of time elapsed since the last reversal, in days.'
  print, 'Alternatively, can enter the string giving the image mid-time.'
  retall
endif

if not keyword_exists(separate) then separate = 1
if not keyword_set(nr) then nr = 2000
if not keyword_set(dr) then dr = .1  ; km/pixel
if not keyword_set(jin) then jin = 0
if not keyword_set(sigma) then sigma = 35.
if not keyword_set(xi_d) then xi_d = 10.
if keyword_set(decay) then begin
  decayprof = exp( -(findgen(nr)*dr/decay)^2/2 )
endif
restore, '/home/borogove/matthewt/idl/iss/jeorbits/jeorbits_revtimes.sav'
nrt = n_elements(revtimes)
revperiod = mean( revtimes_yr[1:nrt-1] - revtimes_yr )*365.25
if not keyword_set(image_mid_time) then image_mid_time = '2004-183T03:41' ; SOI
if not keyword_exists(time) then begin
  year = double(strmid( image_mid_time, 0, 4 ))
  _doy = double(strmid( image_mid_time, 5, 3 ))
  hour = double(strmid( image_mid_time, 9, 2 ))
  min = double(strmid( image_mid_time, 12, 2 ))
  leap = 1-sign( year mod 4 )
  foo = doy( _doy, leap=leap, month=mon, day=day )
  jd = julday( mon[0], day[0], year, hour, min )
  ; Find time since the pre-SOI reversal (2002 JAN 21)
  time = jd - 2451545 - revtimes
  jin = max(where( time ge 0 ))
  time = time[jin]
  jin = jin mod 2  ; 0 for Epimetheus in front, 1 for Janus in front
endif else time = double(time)
if time eq 0 then jin = 1 - jin  ; jin describe most recent wave, not upcoming

; Density wave amplitudes from mass ratio 
; .0357/.1284 = .278 (Cooper and Murray 2004)
; 5.28/19.02 = .278 (Jacobson and French 2004)
ajan = 1.0d0
aepi = 0.278d

; Resonance locations
janus = naifsat( 'Janus' )
epime = naifsat( 'Epimetheus' )
; rres[0,*] = Janus
; rres[1,*] = Epimetheus
; rres[*,0] = current config
; rres[*,1] = other config
; rres[*,2] = rres[*,0]
rres = [[ resloc( rl1, rl2, janus, cm04=1+jin ), $
          resloc( rl1, rl2, epime, cm04=1+jin ) ], $
        [ resloc( rl1, rl2, janus, cm04=2-jin ), $
          resloc( rl1, rl2, epime, cm04=2-jin ) ], $
        [ resloc( rl1, rl2, janus, cm04=1+jin ), $
          resloc( rl1, rl2, epime, cm04=1+jin ) ]]
; Define an array of radius coordinates
rr = min(rres) + findgen(nr)*dr
if keyword_set(radi) then if min(rr) lt min(radi) then begin
  rr = rr[where( rr ge min(radi) )]
  nr = n_elements(rr)
endif
if keyword_set(radi) then if max(rr) gt max(radi) then begin
  rr = rr[where( rr le max(radi) )]
  nr = n_elements(rr)
endif
; Same locations as rres, but in the form of array indices
xres = interpol( findgen(nr), rr, rres )

; Get resonance parameters
mm = rl2 + 1
kk = rl1 - mm
; Get true longitudes of Janus and Epimetheus under various reversal regimes
if not keyword_set(imlon) then imlon = 0
if keyword_set(im_lon_scan) then begin
  xint = [ min(where( radi ge min(rr) )), max(where( radi le max(rr) )) ]
  imlon = mean( im_lon_scan[xint[0]:xint[1]] )  
endif
if keyword_set(zerophase) then begin
  pomegaj = [ 0, 0, 0 ]
  pomegae = [ 0, 0, 0 ]
  phasej = [ 0, 0, 0 ]
  phasee = [ 0, 0, 0 ]
endif else begin
  if kk gt 0 then begin
    pomegaj = [ jeorbits6(jd,2002,sat=610,/silent,/node), $
                jeorbits6(jd,1998,sat=610,/silent,/node), $
                jeorbits6(jd,1994,sat=610,/silent,/node) ]
    pomegae = [ jeorbits6(jd,2002,sat=611,/silent,/node), $
                jeorbits6(jd,1998,sat=611,/silent,/node), $
                jeorbits6(jd,1994,sat=611,/silent,/node) ]
  endif
  phasej = fix_angles( /to360, mm*imlon + kk*pomegaj - $
                       (mm+kk) * [ jeorbits6(jd,2002,sat=610,/silent), $
                                   jeorbits6(jd,1998,sat=610,/silent), $
                                   jeorbits6(jd,1994,sat=610,/silent) ] )
  phasee = fix_angles( /to360, mm*imlon + kk*pomegae - $
                       (mm+kk) * [ jeorbits6(jd,2002,sat=611,/silent), $
                                   jeorbits6(jd,1998,sat=611,/silent), $
                                   jeorbits6(jd,1994,sat=611,/silent) ] )
endelse

; Calculate the group velocity
kappa = sqrt(caviar_kappa2( mean(rres) ))
vgroup = !dpi * 6.672e-8 * sigma / kappa  ; cm/s
vgroup = vgroup / 1e5 * 86400  ; convert to km/day

; Create dw1, a nx2x3 array with the last 3 waves for each of the two moons
dwj0 = fdensity_wave5( rr-rr[0]+rres[0,0], a=ajan, xi_d=xi_d, mm=rl2+1, $
                      sigma=sigma, rres=rres[0,0], phi=phasej[0] )
dwe0 = fdensity_wave5( rr-rr[0]+rres[1,0], a=aepi, xi_d=xi_d, mm=rl2+1, $
                      sigma=sigma, rres=rres[1,0], phi=phasee[0] )
dwj1 = fdensity_wave5( rr-rr[0]+rres[0,1], a=ajan, xi_d=xi_d, mm=rl2+1, $
                      sigma=sigma, rres=rres[0,1], phi=phasej[1] )
dwe1 = fdensity_wave5( rr-rr[0]+rres[1,1], a=aepi, xi_d=xi_d, mm=rl2+1, $
                      sigma=sigma, rres=rres[1,1], phi=phasee[1] )
dwj2 = fdensity_wave5( rr-rr[0]+rres[0,0], a=ajan, xi_d=xi_d, mm=rl2+1, $
                      sigma=sigma, rres=rres[0,0], phi=phasej[2] )
dwe2 = fdensity_wave5( rr-rr[0]+rres[1,0], a=aepi, xi_d=xi_d, mm=rl2+1, $
                      sigma=sigma, rres=rres[1,0], phi=phasee[2] )
dw1 = [[[ interpol( dwj0, rr-min(rres), rr-rres[0,0] ) ], $
        [ interpol( dwe0, rr-min(rres), rr-rres[1,0] ) ]], $
       [[ interpol( dwj1, rr-min(rres), rr-rres[0,1] ) ], $
        [ interpol( dwe1, rr-min(rres), rr-rres[1,1] ) ]], $
       [[ interpol( dwj2, rr-min(rres), rr-rres[0,0] ) ], $
        [ interpol( dwe2, rr-min(rres), rr-rres[1,0] ) ]]]
; Turn waves upside-down if reverse contrast is called for
if keyword_set(reverse) then dw1 = -dw1
; Make sure no wave activity interior to rres
for j=0,1 do for k=0,2 do begin
  foo = where( rr le rres[j,k], count )
  if count gt 0 then dw1[where( rr le rres[j,k] ),j,k] = 0
endfor
if keyword_set(firstonly) then dw1[*,*,1:2] = 0

; Create dw2, which has zeroes at locations where no wave is being driven
if not keyword_set(nrev) then nrev = 100
propdist = vgroup * ( time + findgen(nrev)*revperiod ) / dr  ; in pixels
dw2 = dw1
if propdist[0] ne 0 then propdist = [ 0, propdist ]
for j=0,1 do for k=0,2 do begin
  bb = propdist + xres[j,k]
  bb = [ vec_remove(bb,where( bb ge nr )), nr-1 ]
  if n_elements(bb) lt k+2 then bb = [ bb, replicate(nr-1,k+2-n_elements(bb)) ]
  ;; This was used back when dw1 was nx2x2, w/o phase info before 
  ;; penultimate reversal
  ;for m=1-k,n_elements(bb)-2,2 do dw2[fix(bb[m]+1)>0:fix(bb[m+1]),j,k] = 0
  ;; Now each segment has its own phase, so zero out all but one segment
  dw2[0:fix(bb[k])<(nr-1),j,k] = 0
  if ceil(bb[k+1]) lt nr-1 then dw2[ceil(bb[k+1]):nr-1,j,k] = 0
  if keyword_set(decay) then begin
stop
    if fix(bb[k]) lt nr-1 then begin    ; and k ne 0
      ; Tack a gaussian onto the front of the segment
      bbk = fix(bb[k])
      dw2[0:bbk,j,k] = dw2[bbk+1,j,k] * decayprof[ bbk - indgen(bbk+1) ]
    endif
    if ceil(bb[k+1]) lt nr-1 then begin
      ; Tack a gaussian onto the back of the segment
      bbk1 = ceil(bb[k+1])
      dw2[bbk1:nr-1,j,k] = dw2[bbk1-1,j,k] * decayprof[0:nr-1-bbk1]
    endif
  endif
endfor
if keyword_set(noreversals) then begin
  dw2 = dw1
  dw2[*,*,1:2] = 0
endif
; Make one wavetrain for each satellite
dwjj = total( dw2[*,0,*], 3 )
dwee = total( dw2[*,1,*], 3 )
; Make one total wavetrain
dw = dwjj + dwee
; Decrease resolution
if keyword_set(resample) then begin
  if not keyword_set(dradi) then begin
    dradi = mean( radi[1:n_elements(radi)-1] - radi[0:n_elements(radi)-2] )
  endif
  drr = rr[1] - rr[0]
  if dradi gt drr then begin
    ;run_fft, val[xint[0]:xint[1]], fft_power, fft_f, $
    ;         x=radi[xint[0]:xint[1]], /noplot, coeff=fft_coeff
    ;plot_fft, val[xint[0]:xint[1]], fft_power, fft_f, $
    ;          ps=-4, yr=[0,5], xtit=' ', coeff=fft_coeff, $
    ;          xtickn=replicate(' ',20) ;, xtit='Wavelength (pixels)'
    ;_fft_power = fft_power
    ;for k=1,n_elements(fft_power)-1 do begin
    ;  _fft_power[k] = _fft_power[k-1] + _fft_power[k]
    ;endfor
    ;_fft_power = _fft_power / max(_fft_power)
    ;effscale = interpol( 1/fft_f, _fft_power, 0.9 )
    ;plot, 1/fft_f, _fft_power, ps=-4, /xlog, xtit='Wavelength (pixels)', $
    ;      ytit='Cumulative Fourier Power!Cin Residual Phase', /xs, /ys
    ;oplot, [1e-10,effscale,effscale], [0.9,0.9,1e-10], l=1
    dw = smooth( dw, 2*dradi/drr )
  endif
endif else resample = 0
if keyword_set(noplot) then return

!y.omargin=!y.omargin+[4,2]
!p.multi=[0,1,1]
xtn = replicate('',20)
xtit = 'Radius'+tkmtit()
if keyword_set(separate) then begin
  !p.multi = !p.multi + [0,0,1]
  xtn = [ [replicate(' ',20)], [xtn] ]
  xtit = [ '', xtit ]
endif
if keyword_set(radi) and keyword_set(val) then begin
  !p.multi = !p.multi + [0,0,1]
  xtn = [ [replicate(' ',20)], [xtn] ]
  xtit = [ '', xtit ]
endif
if keyword_set(onebox) then !p.multi = !p.multi + [0,0,2]
if keyword_set(overplot) then !p.multi = 0
if keyword_set(pmulti) then !p.multi = pmulti
nplots = 0

if keyword_set(_tit) then tit=_tit else tit=''
; Red and Cyan are always inner
clr = [ ctred(), ctpurple(), ctblue(), ctcyan() ]
if keyword_set(usegreen) then clr[3] = ctgreen()
if jin eq 1 then index = [1,0,1,3,2,3] else index = [0,1,0,2,3,2]
clr = clr[index]
if !p.charsize eq 0 then !p.charsize=1
if keyword_set(radi) and keyword_set(val) then begin
  if keyword_set(overplot) then begin
    !p.charsize = 1.5
    ys=9
    yma=[(float(!d.y_size)/!d.y_ch_size-6)/3,0]
    chars=!p.charsize
  endif else begin
    ys=1
    yma=[0,0]
    chars=!p.charsize*2
  endelse
  if keyword_set(onebox) then xs = 5 else xs = 1
  ddrr = max(rr) - min(rr)
  ytit = 'Observed I/F'
  if keyword_set(noyaxis) then begin
    ytn = replicate(' ',20)
    if noyaxis eq ' ' then begin
      ytit = ''
    endif else ytit = noyaxis+ytit
  endif else begin
    noyaxis = ''
    ytn = ''
  endelse
  plot, tkm(radi), val, xs=xs, ys=ys, tit=tit, $
      xr=tkm([min(rr)-ddrr/12,max(rr)+ddrr/12]), xtickn=xtn[*,nplots], $
      ytit=ytit, xtit=xtit[nplots], chars=chars, yma=yma, ytickn=ytn, $
      ytick_get=ytg
  if keyword_set(onebox) then axis, xaxis=1, /data, /xs, $
      xtickn=replicate(' ',20), xticki=xticki
  if keyword_set(noyaxis) then begin
    nytg = n_elements(ytg)
    ytg = strtrim(ytg,2)
    lastnonzero = intarr(nytg)
    for j=0,nytg-1 do begin
      lastnonzero[j] = strlen( ytg[j] ) - 1
      while strmid( ytg[j], lastnonzero[j], 1 ) eq '0' do begin
        lastnonzero[j] = lastnonzero[j] - 1
      endwhile
    endfor
    last = max(lastnonzero[ [0,nytg-1] ])
    xyouts, !x.crange[1]-(!x.crange[1]-!x.crange[0])/30, $
            ytg[0]-(!y.crange[1]-!y.crange[0])/30, $
            strmid(ytg[0],0,last+1), align=1, chars=!p.charsize/2
    xyouts, !x.crange[1]-(!x.crange[1]-!x.crange[0])/30, $
            ytg[nytg-1]-(!y.crange[1]-!y.crange[0])/30, $
            strmid(ytg[nytg-1],0,last+1), align=1, chars=!p.charsize/2
  endif
  tit=''
  nplots = nplots + 1
endif
if rl1-rl2 eq 1 then begin
  ; First-order resonances
  plot, tkm([min(rr),max(rr)]), [0.5,4.5], /xs, /ys, /nodata, $
	chars=!p.charsize*2, xtit=xtit[n_elements(xtit)-1], $
	yma=[0,0], ytickle=1e-10, yticki=1, $
	ytickn=['Epime 1', 'Epime 2', 'Janus 1', 'Janus 2' ]
  for j=0,1 do for k=0,2 do begin
    nozero = where( dw2[*,j,k] ne 0 )
    mkexed, nozero, exed, z
    for zz=0,z do polyfill, tkm(rr[exed[[0,1,1,0,0],zz]]), $
	                 [0,0,1,1,0]*.25+(1-j)*2+0.875+k, co=clr[3*j+k];, /fill
  endfor
  goto, finish
endif
if keyword_set(overplot) then begin
  plot, tkm(rr), dw, /xs, /ys, tit=tit, yr=yr, xtickn=xtn[*,nplots], $
        xtit=xtit[nplots], ytickn=replicate(' ',20), yma=yma, $
        /noerase, /nodata, ytickle=1e-10, chars=chars
  oplot, tkm(rr), dw, co=ctgreen()
  axis, yaxis=1, /ys, yr=yr, ytit='Model Wave', chars=!p.charsize*2
endif else begin
  if keyword_set(onebox) then begin
    xs = 5
    ys = 5
    ytit = ''
    ytle = 1e-10
    ytn = replicate(' ',20)
    !p.multi = !p.multi - [!p.multi[1]-1,0,0]
  endif else begin
    xs = 1
    ys = 1
    ytit = 'Model Wave'
  endelse
  plot, tkm(rr), dw, xs=xs, ys=ys, tit=tit, yr=yr, xtickn=xtn[*,nplots], $
        ytit=ytit, xtit=xtit[nplots], yma=[0,0], chars=!p.charsize*2, $
        ytickle=ytle, ytickn=ytn, xr=!x.crange
  if resample eq 2 then oplot, tkm(rr), dwjj+dwee, co=ctyellow()
endelse
nplots = nplots + 1
if keyword_set(separate) then begin
  if keyword_set(overplot) then begin
    yma = [0,(float(!d.y_size)/!d.y_ch_size-6)*2/3]
    noerase = 1
  endif else begin
    yma = [0,0]
    noerase = 0
  endelse
  if keyword_set(onebox) then begin
    xs = 5;9
    !p.multi = !p.multi - [!p.multi[1]-1,0,0]
  endif else begin
    xs = 1
    ytit = 'Model Wave!CComponents'
  endelse
  plot, !x.crange, [ min([dw,reform(dw2,n_elements(dw2))]), $
                     max([dw,reform(dw2,n_elements(dw2))]) ], $
        xs=xs, ys=ys, /nodata, yr=yr, yma=yma, ytickn=ytn, $
	ytit=ytit, xtit=xtit[nplots], chars=!p.charsize*2, ytickle=ytle
endif else begin
  ;oplot, tkm(rr), dwjj, co=ctred()
  ;oplot, tkm(rr), dwee, co=ctpurple()
endelse
legend = [ 'Janus Outer', 'Janus Inner', $
           'Epimetheus Inner', 'Epimetheus Outer' ]
legend = legend[index]
ly = [ 2, 1, 6, 7 ]
ly = ly[index]
choffset = total(abs( !y.crange )) / 25
for j=0,1 do for k=0,2 do begin
  int = where( abs(dw2[*,j,k]) gt 0.1, count )
  if count gt 0 then oplot, tkm(rr[int]), dw2[int,j,k], co=clr[3*j+k]
  oplot, tkm(rres[j,k])*[1,1], !y.crange, co=clr[3*j+k], l=1
  if keyword_set(printright) then begin
    x = !x.crange[1]-(!x.crange[1]-!x.crange[0])/30
    chars = 1.5
    align = 1
  endif else begin
    x = !x.crange[0]+(!x.crange[1]-!x.crange[0])/30
    chars = 1.5
  endelse
  if not keyword_set(nolegend) then xyouts, x, co=clr[3*j+k], $
          !y.crange[1]-(!y.crange[1]-!y.crange[0])/8*ly[3*j+k]-choffset, $
          charsize=chars, legend[3*j+k], align=align
endfor
if keyword_set(onebox) then begin
  if keyword_set(noxaxis) then begin
    xtn = replicate(' ',20)
    xtit[*] = ''
  endif else xtn = ''
  !p.multi = !p.multi + [1,0,0]
  yma1 = [0,-(float(!d.y_size)/!d.y_ch_size/!p.charsize-6)/!p.multi[2]]
  ; Draw axes
  ytit='Model Wave'
  if noyaxis eq ' ' then ytit=''
  plot, tkm(rr), dw, /nodata, ytit=ytit, xtit=xtit[nplots], yma=yma1, $
        chars=!p.charsize*2, xs=9, ytickle=ytle, ytickn=ytn, /noerase, $
        xticki=xticki, xtickn=xtn, xr=!x.crange
  ; Return active plotting window to what it was before last command
  plot, tkm(rr), dw, xs=xs, ys=ys, /nodata, yr=yr, yma=yma, ytickn=ytn, $
	ytit=ytit, xtit=xtit[nplots], chars=!p.charsize*2, ytickle=ytle, $
        xr=!x.crange
endif
if not keyword_set(separate) then oplot, tkm(rr), dw, thick=2
if not keyword_set(nowrite) then begin
  xyouts, !x.crange[1]-(!x.crange[1]-!x.crange[0])/30, $
          !y.crange[1]-(!y.crange[1]-!y.crange[0])/8, charsize=1, align=1, $
          'Sigma = '+strtrim(sigma,2)+'!CXi_d = '+strtrim(xi_d,2)+$
          '!CJphase = ('+strtrim(fix(phasej[0]),2)+$
          ','+strtrim(fix(phasej[1]),2)+','+strtrim(fix(phasej[2]),2)+$
          ')!CEphase = ('+strtrim(fix(phasee[0]),2)+$
          ','+strtrim(fix(phasee[1]),2)+','+strtrim(fix(phasee[2]),2)+')'
endif

finish:

!y.omargin=!y.omargin-[4,2]

;stop
end
