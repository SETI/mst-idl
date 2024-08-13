rmin = -4.
rmax = 4.
ndec = rmax - rmin
nperdec = 10
rr = 10^( findgen(ndec*nperdec+1)/nperdec + rmin )
nrr = n_elements(rr)
r2nmin = -15.5
r2nmax = -4.5

broad = 0
!p.multi = [0,2,2]
device, decomposed=0
if keyword_set(dolzr) then begin
  psname = 'rhearpx_fig'
  if keyword_set(column) then psname = psname + '_column'
  lzr, psname
  @plot_prepare
  plot_color
  device, /cmyk
endif
tvlct,r,g,b,/get
sub = lindgen(249)*(256/249)
ll = 168
r = [ r[sub], 0,   255, 0,   ll,  255, ll,  255 ]
g = [ g[sub], 0,   0,   255, ll,  ll,  255, 255 ]
b = [ b[sub], 255, 0,   0,   255, ll,  ll,  255 ]
tvlct, r, g, b
clr = !d.table_size - [ 7, 6, 5 ]
clrlt = !d.table_size - [ 4, 3, 2 ]
next:

dust_cutoff = 1e-2
if keyword_set(broad) then begin
  pen_depth = 9e-4 ;cm
  r2n_iss_dust = 2.76e-15 ;cm^{-1}
  r2n_iss_large = 2.96e-12 ;cm^{-1}
  r2n_mimi1 = 9e-14 * 4 * !pi / 3 / rr
  r2n_mimi2 = replicate( 4.19e-10, nrr )
  tit = 'Broad Cloud'
  kev = '25'
endif else begin
  pen_depth = .0824 ;cm
  r2n_iss_dust = 6.4e-14 ;cm^{-1}
  r2n_iss_large = 2.74e-12 ;cm^{-1}
  r2n_mimi1 = 2.84e-9 / rr
  r2n_mimi2 = replicate( 3.37e-8, nrr )
  tit = 'Narrow Rings'
  kev = '300'
endelse
r2n_mimi = r2n_mimi1 > r2n_mimi2

th = 4
if keyword_set(column) and broad eq 1 then !p.multi[0] = !p.multi[0] - 1
plot, 10^[rmin,rmax], 10^[r2nmin,r2nmax], /nodata, xs=5, ys=5, /xlog, /ylog, $
      tit=tit
polyfill, [ rr, 10^!x.crange[[1,0]] ], $
          [ r2n_mimi, 10^!y.crange[[1,1]] ], co=clrlt[1]
if keyword_set(broad) then begin
  xyouts, 1e1, 3*1.5e-7, align=0.5, co=clr[1], chars=1.5, $
          'Required to account!Cfor observed!Celectron absorptions'
endif else begin
  xyouts, 1e1, 3*7e-7, align=0.5, co=clr[1], chars=1.5, $
          'Required to account for!Cobserved electron absorptions'
endelse
polyfill, [ 10^!x.crange[0], dust_cutoff, dust_cutoff, 10^!x.crange[[1,1,0]] ],$
          [ r2n_iss_dust[[0,0]], r2n_iss_large[[0,0]], 10^!y.crange[[0,0]] ], $
          co=clrlt[2]
xyouts, 1e1, 3*2e-14, align=0.5, co=clr[2], chars=1.5, $
        'Allowed by!Cimaging non-detection'
oplot, [ 10^!x.crange[0], dust_cutoff ], r2n_iss_dust[[0,0]], $
       co=clr[2], thick=th
oplot, [ dust_cutoff, 10^!x.crange[1] ], r2n_iss_large[[0,0]], $
       co=clr[2], thick=th
oplot, rr, r2n_mimi, co=clr[1], thick=th
foo = where( rr gt pen_depth )
oplot, rr[foo], r2n_mimi1[foo], l=2, co=clr[1], thick=th

oplot, [dust_cutoff,dust_cutoff], 10^!y.crange, l=1, co=clr[2]
xyouts, dust_cutoff/10^.08, 10^mean(!y.crange), orient=90, align=0.5, $
        co=clr[2], 'diffraction/reflection transition!Cfor light-scattering'
oplot, [pen_depth,pen_depth], 10^!y.crange, l=1, co=clr[1]
xyouts, pen_depth/10^.08, 10^mean(!y.crange), orient=90, align=0.5, $
        co=clr[1], 'electron penetration depth!Cat '+kev+' keV'
if keyword_set(plotcheck) then begin
  if keyword_set(broad) then begin
    oplot, [0.130,0.130], 10^!y.crange, l=1
  endif else begin
    oplot, [7.77e2,7.77e2], 10^!y.crange, l=1
  endelse 
endif

notn = replicate(' ',20)
axis, xaxis=0, /xs, /xlog, xtit='Particle radius, r (cm)'
axis, xaxis=1, /xs, /xlog, xtickn=notn
;axis, yaxis=0, /ys, /ylog, ytit='Total particle cross-section area!Cper unit path length, !Mpr!U2!Nn (cm!U-1!N)'
axis, yaxis=0, /ys, /ylog, ytit='Extinction coefficient, !Mpr!U2!Nn (cm!U-1!N)'
axis, yaxis=1, /ys, /ylog, ytickn=notn

if not keyword_set(broad) then begin
  broad = 1
  goto, next
endif

if keyword_set(dolzr) then clzr

end

