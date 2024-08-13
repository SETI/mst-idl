; This isn't quite right.  Make the boundary between the lobes more precise by finding the minimum between the two peaks.  Also, exclude frequencies even higher or lower than the peaks.  Also, do this on the Prometheus 9:8 rather than Pandora 8:7

nn = 1120-800+1
intpd = indgen(nn) + 800
lline = interpol( [.583,1.38,2.33], [751,802,837], r[intpd] )

fund = fltarr(nn)
harm = fltarr(nn)
for jj=0,nn-1 do begin
  fund[jj] = total( real_part(wave[jj,where(period lt lline)]) )
  harm[jj] = total( real_part(wave[jj,where(period gt lline)]) )
;  lfphase[jj] = get_phase
endfor
window, 1
!p.multi=[0,2,3]
plot, r[intpd], _v[intpd], tit='Real Wave', /xs, /ys, xtit=xtit
plot, r[intpd], total( real_part(wave[intpd,*]), 2 ), /xs, /ys, xtit=xtit, $
	tit='Reconstructed Wave'
plot, r[intpd], fund, tit='Reconstructed from Low-Freq Peak', /xs, /ys, xtit=xtit
plot, r[intpd], harm, tit='Reconstructed from High-Freq Peak', /xs, /ys, xtit=xtit
;plot, r[intpd], 

end
