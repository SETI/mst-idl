_totphase = unwrap_phase(totphase)
;window, 1, xs=768, ys=768
if !d.name eq 'X' then wset, 1
!p.multi = [0,1,2]
plot, r, _totphase, /xs, /ys, xtit=xtit, ytit='Phase (degrees)', th=2

; Fitted peaks from Janus 2:1
restore, '~/idl/iss/density_wave/ppeaks_j21.sav'
npp = n_elements(ppeaks)
ind = indgen( (npp+1)/2 )*2
ppeaks = ppeaks[ ind ]
pphase = 180 * (ind+1)
npp = n_elements(ppeaks)
_totphase = interpol( pphase, ppeaks, r )

xx0 = 0 & xx1 = nt
;if imn eq 10 then xx0 = 425
if imn eq 10 then xx1 = 775
if imn eq 24 then xx1 = 550
if imn eq 28 then xx0 = 300;168
if imn eq 28 then xx1 = 550
if imn eq 28 then xx0 = 350
if imn eq 28 then xx1 = 500
if keyword_set(atlas54) and imn eq 15 then xx0 = r1
if keyword_set(atlas54) and imn eq 15 then xx1 = r2
if imn eq 17 then xx0 = 70
if imn eq 17 then xx1 = 135
if keyword_set(some_wave) then xx0 = 300
if keyword_set(some_wave) then xx1 = 750
int1 = findgen(xx1-xx0) + xx0
fit1 = poly_fit(r[int1],_totphase[int1],2)
oplot,r,poly(r,fit1),l=1;co=yellow()
oplot,r[int1],_totphase[int1],co=red()
zpt1 = -fit1[1]/fit1[2]/2
oplot,[zpt1],[poly(zpt1,fit1)],ps=4;,co=yellow()
rres = thoukm+zpt1
if keyword_set(rres) then print,'Calculated sigma = '+strtrim(2*!pi/3.08/rres^4*60330.^4*(m-1)/(fit1[2]*2*!pi/180),2)
if imn eq 10 then begin
  xx0 = 821 & xx1 = 1274
  int2 = findgen(xx1-xx0) + xx0
  fit2 = poly_fit(r[int2],_totphase[int2],2)
  oplot,r,poly(r,fit2),l=1;co=cyan()
  oplot,r[int2],_totphase[int2],co=red()
  zpt2 = -fit2[1]/fit2[2]/2
  oplot,[zpt2],[poly(zpt2,fit2)],ps=4;,co=yellow()
  rres = thoukm+zpt2
  if keyword_set(rres) then print,'Calculated sigma = '+strtrim(2*!pi/3.08/rres^4*60330.^4*(m-1)/(fit2[2]*2*!pi/180),2)
endif

totk = deriv(r,_totphase)*!pi/180
plot, r, totk, /xs, /ys, yr=yr, xtit=xtit, ytit='Wavenumber (radians/km)'
oplot, r[int1], totk[int1], co=red()
kfit1 = poly_fit(r[int1],totk[int1],1)
oplot, r, poly(r,kfit1), co=yellow()
if keyword_set(some_wave) then begin
  rres = 128000.
  m = 9
endif
if keyword_set(rres) then print,'Calculated sigma = '+strtrim(2*!pi/3.08/rres^4*60330.^4*(m-1)/kfit1[1],2)
if imn eq 10 then begin
  oplot, r[int2], totk[int2], co=red()
  kfit2 = poly_fit(r[int2],totk[int2],1)
  oplot, r, poly(r,kfit2), co=cyan()
  if keyword_set(rres) then print,'Calculated sigma = '+strtrim(2*!pi/3.08/rres^4*60330.^4*(m-1)/kfit2[1],2)
endif

end
