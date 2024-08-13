kk = findgen(91)/10-4
nchi = 5
pmass = findgen(nchi) + 1.5
chi = 0.01 * pmass^(1./3)

if keyword_set(dolzr) then begin
  lzr, 'ScatProbPlot'
  @plot_prepare
endif
!y.margin = 0
!y.omargin = [4,2]
!p.multi = [0,3,2]
!p.charsize = 2
!x.margin = [6.5,3]

redo = 0
xr = [-4.5,5.5]
yr = [-.28,.35]
xtn = ''
xtit='Surface density power law index, k'
redo1:
plot, xr, yr, /nodata, /xs, /ys, ytit='(P!Uout!N-P!Uin!N)/P!Uout!N', $
      xtickn=xtn, xtit=xtit
for j=0,nchi-1 do begin
  oplot, kk, - 49./30*chi[j] + 33./10*(kk+1)*chi[j], co=200-150.*j/nchi
endfor
if keyword_set(redo) then goto, redo2
xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.05, $
        !y.crange[1] - (!y.crange[1]-!y.crange[0])*.1, '(a)', chars=1.5

plot, xr, [-.09,.49], /nodata, /xs, /ys, ytit='(M!Uout!N-M!Uin!N)/M!Uout!N', $
      xtit=xtit
for j=0,nchi-1 do begin
  oplot, kk, 343./60*chi[j] + 33./10*(kk+1)*chi[j], co=200-150.*j/nchi
endfor
xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.05, $
        !y.crange[1] - (!y.crange[1]-!y.crange[0])*.1, '(b)', chars=1.5
oplot, !x.crange, [.14,.14], l=1
oplot, [-.5,-.5], !y.crange, l=1

xr = [-.6,.2]
yr = [-.005,.03]
redo = 1
goto, redo1
redo2:
xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.05, $
        !y.crange[1] - (!y.crange[1]-!y.crange[0])*.1, '(c)', chars=1.5

if keyword_set(dolzr) then clzr

end
