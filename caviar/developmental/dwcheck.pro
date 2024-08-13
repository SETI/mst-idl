!p.multi=[0,3,3]
for j=0,0 do begin
;rres = 127362.93
;_phase0 = 1356.2829
;mm = 9
rres=126798.25;128000.
vma=-5.0704454e-5;-.01
xi_d = 7.5;10
sigma = 41.016977;43.8
_phase0 = 1973.9298;j*40;0
mm = 15;9
;radi1 = findgen(1000)/10 + rres
radi1 = findgen(1150)*.16079922 + rres

fit1 = dblarr(3)
thoukm = get_thoukm(rres)
fit1[2] = 2*!pi/3.08/rres^4*60330.^4*(mm-1)/(sigma*2*!pi/180)
fit1[0] = fit1[2]*(rres-thoukm)^2 + _phase0
fit1[1] = -2*(rres-thoukm)*fit1[2]
;print,fit1

if keyword_set(compensate) then begin
  d1=0
  d2=-_phase0
  d3=+_phase0-315
endif else begin
  d1=0
  d2=0
  d3=0
endelse
if j eq 8 then d3=d3+360
print, -vma/10, xi_d, mm, rres, sigma
vmodel = fdensity_wave5( radi1, a=-vma/10, xi_d=xi_d, mm=mm, phi=45+_phase0, rres=rres, sigma=sigma, xiout=xiout )
;oplot, radi1-thoukm, vmodel, co=red()  ; This reproduces some_wave.pro
run_wavelet, radi1, vmodel, vwave, /noplot
vphase = unwrap_phase(get_phase( total(vwave,2), wrap=vwrap) )
plot, radi1-thoukm, xiout^2/2*180/!pi, l=2, xr=[40,60]+rres-thoukm, tit=strtrim(_phase0,2)
;plot, radi1-thoukm, xiout^2/2*180/!pi, l=2, xr=[0,30], yr=[-200,500], /ys
;plot, radi1-thoukm, xiout^2/2*180/!pi, l=2
oplot, radi1-thoukm, poly(radi1-thoukm,fit1)+d2, co=green()
oplot, radi1-thoukm, vphase+d3+_phase0-(_phase0 mod 360), co=cyan()

endfor

end
