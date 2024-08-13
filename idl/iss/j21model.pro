;pro j21model, sigma=sigma, roffset=roffset, poffset=poffset

if not keyword_set(sigma) then sigma=60.
if not keyword_set(roffset) then roffset=0.
if not keyword_set(poffset) then poffset=0.

rres1 = 235.4 ;+96000 km
rres2 = 247.7 ;+96000 km
kappa = 0.000205201 ;s^-1, obtained from resloc_kappa2 for r=96240.
a = 180./3.08/96240.^4*60330.^4/sigma
groupv = !pi * 6.672e-8 * sigma / kappa * 86400 / 1e5 ; km/day

dt1 = 880. ; # of days from last reversal to SOI
dt2 = 880. + 1461. ; reversal frequency is almost exactly 4 yr (1461 dy)
dt3 = 880. + 1461.*2

; Fits from previous analysis of Janus 2:1
j21fit1 = [ 6224.0754, -56.460157, 0.12825649 ]
j21fit2 = [ 11797.418, -78.943402, 0.14906760 ]

; Fitted peaks from Janus 2:1
restore, '~/idl/iss/density_wave/ppeaks_j21.sav'
npp = n_elements(ppeaks)
ind = indgen( (npp+1)/2 )*2
ppeaks = ppeaks[ ind ] + roffset
pphase = 180 * (ind+1) + poffset
npp = n_elements(ppeaks)

r0 = 225
r1 = max(ppeaks) - max(ppeaks) mod 50 + 50
nr = r1-r0+1
r = findgen(nr) + r0

plot, ppeaks, pphase, ps=4, xtit='Radius - 96000 km', ytit='Phase (deg)', $
        /xs, xr=[r0,r1]
oplot, rres1*[1,1], [0,1e10], l=1
oplot, rres2*[1,1], [0,1e10], l=1

if keyword_set(dofitc) then begin
  c1 = indgen(3)
  c2 = indgen(3)+3
  c3 = indgen(npp-6)+6
  fitc = fltarr(1,3)
  for jj=0,2 do begin
    if jj eq 0 then begin
      c = c1
      clr = ctgreen()
    endif else if jj eq 1 then begin
      c = c2
      clr = ctred()
    endif else begin
      c = c3
      clr = ctcyan()
    endelse
    oplot, ppeaks[c], pphase[c], ps=4, co=clr
    fitc1 = poly_fit( ppeaks[c], pphase[c], 2 )
    oplot, deltar+250, poly(deltar+250,fitc1), co=clr, l=1
    fitc = [ fitc, fitc1 ]
  endfor
  fitc=fitc[1:3,*]
stop
endif

q = [ [a/2*(r-rres2)^2], $
      [a/2*(r-rres1)^2], $
      [a/2*(r-rres2)^2] ]

; Indices for reversals
rev = [ interpol( indgen(nr), r, rres2 ), $
        interpol( indgen(nr), r, groupv*dt1+rres2 ), $
        interpol( indgen(nr), r, groupv*dt2+rres1 ), $
        interpol( indgen(nr), r, groupv*dt3+rres2 ) ]

acphase = fltarr(nr)
clr = [ ctgreen(), ctred(), ctcyan() ]
for jj=0,2 do begin
  ndp = 0
  if jj ne 0 then begin
    dp = q[rev[jj]+1,jj] - q[rev[jj],jj-1]
    while dp lt 0 do begin
      ndp = ndp + 1
      dp = dp + 360
    endwhile
    while dp gt 360 do begin
      ndp = ndp - 1
      dp = dp - 360
    endwhile
  endif
  rv0 = rev[jj]+1
  rv1 = rev[jj+1]<(nr-1)
  acphase[rv0:rv1] = q[rv0:rv1,jj] + ndp*360
  oplot, r, q[*,jj]+ndp*360, l=1, co=clr[jj]
endfor
oplot, r, acphase

end
