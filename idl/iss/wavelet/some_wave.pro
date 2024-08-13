if !d.name eq 'X' then device, decomposed=0
if !d.name eq 'PS' then !p.font = 1; else !p.font = -1
if !d.name eq 'PS' then !p.thick = 2 else !p.thick = 1
if !d.name eq 'PS' then !p.charsize = 1.5; else !p.charsize = 1
dir = '/home/borogove/iss/images/SOI/SOISPTURN/'
!p.charsize=1.5
;radscan_long = 1
wavelength = 0
wake = 0
yr = 0
plotsep = 1
s0x = 1
tit = ''
minlam = 2.39865 & maxlam = 49.4469 & j = 100
param = 6
rpan = 133584.6
lambda = [ 1, 2, 5, 10, 15, 30, 60 ]
nwakes = 0
rsxr = 0
plot_ring_rads = 1
whichrr =  0
order1 = 0
atlas54 = 0
p = 0
cc1 = 0
cc2 = 0

; Some wave
imn = 0
some_wave = 1
minlam = 3.5
m = 9
rres = 128000./60330
sigma = 43.8

if not keyword_set(p) then p = [ -6.5, -3.5, 7 ]
levels = findgen(p[2])*(p[1]-p[0])/(p[2]-1)+p[0]
;levels = [ p[0] - reverse((findgen(10)+1)/1e5), levels ]
if not keyword_set(cc1) then cc1 = 100
if not keyword_set(cc2) then cc2 = 220
c_colors = cc2-findgen(p[2])/(p[2]-1)*(cc2-cc1)

if not keyword_set(vgr) then restore, dir+'stretch.sav'

image_name=dir+filenames[imn]
.run restore_radscan
if keyword_set(radscan_long) then xs = 1500 else xs = 640
if !d.name eq 'X' and keyword_set(radscan_long) then window, xs=xs, ys=512
if keyword_set(vgr) then radi = *data.radius
if keyword_set(vgr) then val = *data.tau
if keyword_set(some_wave) then restore, '~/idl/iss/density_wave/some_wave.sav'
thoukm = min(radi) - (min(radi) mod 1000)
if imn eq 34 then rsxr = rsxr + 2000
if imn eq 34 then thoukm = min(radi) - (min(radi) mod 10000)
if keyword_set(rsxr) then radscan_xr=rsxr else radscan_xr=[min(radi),max(radi)]-thoukm
;.run plot_radscan

x0 = max(where( radi-thoukm le radscan_xr[0], count ))
if x0 ne 0 then x0 = x0 - 1
if count eq 0 then x0 = radi[0]-thoukm
x1 = max(where( radi-thoukm lt radscan_xr[1], count ))
r = radi[x0:x1]-thoukm
if imn eq 27 then r = r - 20
if imn eq 29 then r = r + 10
if imn eq 33 then r = r + 21.5
if imn eq 34 then r = r + 30;15
v = val[x0:x1]
nt = n_elements(r)
dt = mean( r[1:nt-1] - r[0:nt-2] )
;s0 = 2*dt*s0x
;dj = .125/2
;j = alog2(nt*dt/s0)/dj/1.5
;s0 = alog2(minlam)
;dj = (alog2(maxlam)-s0)/(j-1)
fourier_factor = 4 * !pi / (param+sqrt(2+param^2))
s0 = minlam / fourier_factor
dj = alog2(maxlam/minlam) / j
xtit='Radius - '+strtrim(long(abs(thoukm)),2)+' km'
yr = 2 * !pi / fourier_factor / s0 / [ 2.^(j*dj), 1 ]
s0 = dt
dj = 0.125
j = alog2(float(nt)/2)/dj
if j ne fix(j) then dj = alog2(float(nt)*dt/s0)/float(fix(j)+1)
j = fix(j) + 1

vvv = v
recon = 1;0
wave = wavelet( vvv, dt, /pad, s0=s0, j=j, dj=dj, param=param, period=period, scale=scale, coi=coi, signif=signif, recon=recon, dof=dof )

fft_theor = signif / (chisqr_cvf(.05,dof)/dof) / stddev(v)^2
_period = period
vv = (v-min(v))/(max(v)-min(v))*(max(period)-min(period))/4+min(period)
vv = 2.^vv
if not keyword_set(wavelength) then period = 2*!pi / period
if not keyword_set(wavelength) then coi = 2*!pi / coi
if keyword_set(frequency) then period = period / 2 / !pi
if not keyword_set(wavelength) then vv = max(period) - (v-min(v))/(max(v)-min(v))*(max(period)-min(period))/4
if keyword_set(wavelength) then ytit='Wavelength (km)' else  ytit='Wavenumber (radians/km)'
if keyword_set(wavelength) then ys=1 else ys=9
if keyword_set(wavelength) then xma=[10,3] else xma=[10,6]
if keyword_set(wake) then r = r + thoukm - rpan
;if keyword_set(wake) then xtit = 'Radial Distance from Pan (km)
if keyword_set(wake) then wakepredict = 2*rpan*theta/3/r^2
if nwakes gt 1 then for jj=1,nwakes-1 do wakepredict = [ [wakepredict], [2*rpan*(theta+jj*2*!pi)/3/r^2] ]
if keyword_set(fakewake) then wakepredict = [ [wakepredict], [2*rpan*(theta+.6666*2*!pi)/3/r^2] ]
if keyword_set(fakewake) then nwakes = nwakes + 1
if keyword_set(wake) then r = r - thoukm + rpan
if keyword_set(wavelength) then wakepredict = 2*!pi/wakepredict
if keyword_set(frequency) then wakepredict = wakepredict / 2 / !pi
if keyword_set(plotsep) then yma=[4,-15/!p.charsize] else yma=[4,2]
if keyword_set(plotsep) then !p.multi=[0,1,2] else !p.multi=0
if keyword_set(vgr) then _v = smooth(v,5) else _v=v
if imn eq 34 then yti = .004
if keyword_set(plotsep) then plot, r, _v, /xs, /ys, xma=xma, yma=[15/!p.charsize,2], $
         xtickn=replicate(' ',20), ytit='I/F', tit=tit, yticki=yti
if keyword_set(plotsep) then tit=''
if not keyword_set(yr) then yr = [ min(period), max(period) ]
loadct, 0
;contour, (abs(wave^2))^(1./4), r, period, /xs, ys=ys, xma=xma, yma=yma, $
contour, alog10(abs(wave^2)), r, period, /xs, ys=ys, yr=yr, xma=xma, yma=yma, $
         /fill, nlevels=15, ylog=wavelength, xtit=xtit, ytit=ytit, tit=tit, $
	 levels=levels, c_colors=c_colors
signif = rebin(transpose(signif),n_elements(r),n_elements(period))
;if param eq 6 then cdelta = 0.776
;psi0 = !pi^(-0.25)
;suse = where( scale gt s0*2 )
;recon = dj*sqrt(dt)/(cdelta*psi0)*( float(wave[*,suse]) # (1./sqrt(scale[suse])) ) + mean(v)
;resid = v - recon
;_aa = fltarr(4)
;_aa[0] = mean(resid)
;_aa[1] = ( max(resid) - min(resid) )/2
;_aa[2] = !pi / ( r[where(resid eq max(resid))] - r[where(resid eq min(resid))] )
;_aa[3] = -_aa[2] * r[where(resid eq min(resid))]
;weights = replicate(1.,n_elements(r))
;sfit = curvefit( r, resid, weights, _aa, _aa_sigma, function_name='fsine' )
;recon = recon + sfit
;noise = v - recon
;sdev = stddev(noise)^2
;;sdev = stddev(v)^2  ; For comparison with signif
;siglvl = [ 0.95 ]
;c_annot = string(siglvl*100,fo='(F4.1)')+'%'
;_signif = fltarr(1,j+1,n_elements(siglvl))
;for jj=0,n_elements(siglvl)-1 do _signif[*,*,jj] = fft_theor*sdev*chisqr_cvf(1.-siglvl[jj],dof)/dof
;_signif = rebin( _signif, nt, j+1, n_elements(siglvl) )
;;for jj=0,n_elements(siglvl)-1 do contour, (abs(wave)^2)/rebin(transpose(fft_theor),nt,j+1), r, period, /overplot, level=sdev*chisqr_cvf(1.-siglvl[jj],dof)/dof, c_annot=c_annot[jj]
;;for jj=0,n_elements(siglvl)-1 do contour, (abs(wave)^2)/_signif, r, period, /overplot, level=1, c_annot=c_annot[jj]
;;contour, (abs(wave)^2)/signif, r, period, /overplot, level=1, c_annot='95%'
plots, r, coi, noclip=0, color=180
for jj=-1,1,2 do polyfill, [r[0],r,r[nt-1],r[[nt-1,0,0]]], [yr[1],coi,yr[1],yr[[0,0,1]]], /line_fill, noclip=0, orient=jj*45, /data, spacing=.25, co=180
if not keyword_set(wavelength) then axis, yaxis=1, yticks=n_elements(lambda)-1, $
         ytickv = 2*!pi/lambda, ytickn = strtrim(lambda,2), /ys, $
         ytit='Wavelength (km)'
if not keyword_set(plotsep) then oplot, r, vv

if keyword_set(plot_ring_rads) then jimg = imn
.run restore_ring_rads
if keyword_set(plot_ring_rads) and keyword_set(whichrr) then ring_rads=ring_rads[whichrr]
if keyword_set(plot_ring_rads) and keyword_set(whichrr) then ring_rads_legend=ring_rads_legend[whichrr]
if imn eq 27 then ring_rads = [ ring_rads, resloc(9,8,3) ]
if imn eq 27 then ring_rads_legend = [ 'Mi 7:4 (ee''!U2!N)', ring_rads_legend[1:n_elements(ring_rads)-1], 'Ep 9:7!C' ]
if imn eq 33 then zerorr = (where(ring_rads_legend eq 'Pan 66:65'))[0]
if imn eq 33 then ring_rads_legend = ring_rads_legend[zerorr:zerorr+40]
if imn eq 33 then ring_rads = ring_rads[zerorr:zerorr+40]
if imn eq 33 then ring_rads_legend[[16,27]] = '!C'+ ring_rads_legend[[16,27]]
if imn eq 33 and n_elements(ring_rads) ne 41 then stop, 'Ring_rads has changed!'
if imn eq 33 then ring_rads = ring_rads[15:31]
if imn eq 33 then ring_rads_legend = ring_rads_legend[15:31]
if imn eq 33 then ring_rads = [ ring_rads, resloc(11,10,3) ]
if imn eq 33 then ring_rads_legend = [ ring_rads_legend, 'Ep 11:9' ]
if imn eq 39 then ring_rads = vec_remove( ring_rads, [7,8] )
if imn eq 39 then ring_rads_legend = vec_remove( ring_rads_legend, [7,8] )
if imn eq 34 then dd = .05 else dd = 0
if keyword_set(plot_ring_rads) then solid_diamonds
if keyword_set(plot_ring_rads) then oplot, ring_rads-thoukm, replicate(yr[0]+.05+dd,n_elements(ring_rads)), ps=8, noclip=1
if imn eq 33 or imn eq 39 then align = 0 else align = .5
if imn eq 33 or imn eq 39 then orientation = 30 else orientation = 0
if keyword_set(plot_ring_rads) then xyouts, ring_rads-thoukm, replicate(yr[0]+.1+dd,n_elements(ring_rads)), ring_rads_legend, chars=!p.charsize/2, align=align, orientation=orientation
;if imn eq 27 then oplot, [_ring_rads[144]-thoukm], [yr[0]+.05], ps=8, noclip=1
;if imn eq 27 then xyouts, _ring_rads[144]-thoukm+5, [yr[0]+.05], 'Mi 7:4 (eI''!U2!N)', chars=!p.charsize/2
if imn eq 27 then oplot, [_ring_rads[146]-thoukm], [yr[0]+.05], ps=8, noclip=1
if imn eq 27 then xyouts, _ring_rads[146]-thoukm+5, [yr[0]+.05], _ring_rads_legend[146], chars=!p.charsize/2
;Can also plot resonance location at rsxr[0]-60+thoukm and if imn eq 27 then arrow, ring_rads[0]-thoukm-[5], yr[0]+[.05], ring_rads[0]-thoukm-[15], yr[0]+[.05], /data, hsize=5

;; Print with explanatory colors for Prometheus 9:8
;if imn eq 28 then axis, xaxis=0, /xs, xtit=xtit, co=purple()
;if imn eq 28 then axis, yaxis=0, /ys, ytit=ytit, co=green()
;if imn eq 28 then axis, yaxis=1, yticks=n_elements(lambda)-1, $
;         ytickv = 2*!pi/lambda, ytickn = strtrim(lambda,2), /ys, $
;         ytit='Wavelength (km)', co=cyan()
;if imn eq 28 then if keyword_set(plot_ring_rads) then oplot, ring_rads-thoukm, replicate(yr[0]+.05,n_elements(ring_rads)), ps=8, noclip=1, co=red()
;if imn eq 28 then if keyword_set(plot_ring_rads) then xyouts, ring_rads-thoukm, replicate(yr[0]+.1,n_elements(ring_rads)), ring_rads_legend, chars=!p.charsize/2, align=align, orientation=orientation, co=red()


; At each location, the peak wavenumber as weighted by wavelet power.
kmax = rebin( reform(alog(period),1,j+1), nt, j+1 ) * abs(wave^2)
kmax = total(kmax,2) / total(abs(wave^2),2)
kmax = exp(kmax)
;oplot, r, kmax
; ...or, the derivative of the weighted average phase for each location.
;if keyword_set(_totphase) then oplot, r, deriv(smooth(_totphase,150,/edge))/(r[1]-r[0])*!pi/180
;if keyword_set(_totphase) then oplot, r, deriv(_totphase)/(r[1]-r[0])*!pi/180

if keyword_set(kfit1) then oplot, r[xx0:xx1], poly( r[xx0:xx1], kfit1 )
;if keyword_set(kfit1) and imn eq 28 then oplot, r[xx0:xx1], 2*poly( r[xx0:xx1-75], kfit1 ), l=3
if keyword_set(some_wave) then oplot, r, 2*!pi*(m-1) / (3.08*rres^4*sigma) * r, l=2
; Pan 80:79 through Pan 91:90
if imn eq 33 then jind = [2,3,4,5,6,7,8,10,11,12,13,14];9,17
if imn eq 33 then jmax = n_elements(jind)
if imn eq 33 then xxx0 = fltarr(jmax)
if imn eq 33 then for j=0,jmax-1 do xxx0[j]=max(where(r lt ring_rads[jind[j]]-thoukm))+25
if imn eq 33 then xxx1 = xxx0 + 40
if imn eq 33 then mm = [ indgen(jmax)+80 ]
; Pandora 11:10 for j=0, Prometheus 15:14 for j=1
if imn eq 34 then xxx0 = [ 350, 750 ]
if imn eq 34 then xxx1 = [ 650, 1000 ]
if imn eq 34 then mm = [ 11, 15 ]
if imn eq 34 then jind = [0,1]
if imn eq 34 then jmax = 2
sigma = 43
if imn eq 33 or imn eq 34 then for j=0,jmax-1 do oplot, r[xxx0[j]:xxx1[j]], poly( r[xxx0[j]:xxx1[j]], 2*!pi/3.08/ring_rads[jind[j]]^4*60330.^4*(mm[j]-1)/sigma *[-ring_rads[jind[j]]+thoukm,1] ), l=4
if imn eq 34 then xyouts, 133160-thoukm, 5.9, '702!Eo'
if imn eq 34 then xyouts, 133235-thoukm, 4.2, '342!Eo'

.run atlas54
.run wavelet_wake
.run wavelet_animate
.run j21
