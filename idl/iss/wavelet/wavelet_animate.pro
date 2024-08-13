if keyword_set(animate) then begin
window, 3, xs=1024, ys=1024
!p.multi = [0,1,3]
pch = 2.
aa = where( abs(totphase[1:nt-1]-totphase[0:nt-2]) gt 90 )
for xx=0,xxmax-1 do begin
  plot, r, _v, /xs, /ys, yma=[15/pch,2], xtit=xtit, ytit='I/F', chars=pch
  ;oplot, r[aa], _v[aa], ps=4, co=red()
  oplot, r[xx]*[1,1], [0,100], l=1
  plot, period, abs(wave[xx,*]^2), xr=yr, yr=[1e-8,1e-3], /ylog, chars=pch, $
	ytit='Intensity', tit='r = '+strtrim(r[xx],2), yma=[1/pch,2], $
	xtickn=replicate(' ',20)
  for jj=0,nwakes-1 do oplot, wakepredict[xx,jj]*[1,1], [1e-10,1e10], l=1
  plot, period, phase[xx,*], ps=4, xr=yr, yr=[0,360], /xs, /ys, chars=pch, $
	yma=[4,-1/pch], yticki=45, ytit='Phase', xtit='Wavenumber'
  ;if xx ge 250 then wait, 1
  wait, .05
endfor
endif

end

