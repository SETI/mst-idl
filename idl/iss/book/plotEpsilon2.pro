;pro plotEpsilon2, _EXTRA=_EXTRA

if keyword_set(dolzr) then begin
  lzr, 'Porco08_Fig22_mod'
  @plot_prepare
  plot_color
  device, /cmyk
endif
!p.multi = [0,3,2]
!p.charsize = 2

powers = [-0.14, -0.234]
vBs = [0.01, 0.0077]
;vstar = [0.01, 0.001]
vstar = 0.001
colorstep = 50
colors= findgen(n_elements(vstar)+n_elements(powers)+1)*colorstep + 50
N = 100000
v = (findgen(N)+1)/(N/10.0)
linen=1

;if(is_ps()) then colors=replicate(0, n_elements(colors))

;colors=replicate(0, n_elements(colors))
;if !d.name eq 'X' then colors[*] = ctwhite()
colors = [ ctred(), ctblue(), ctgreen() ]

plot, [0.003, 10.0], [0, 1.1],$
      /nodata, $
      /xlog, $
      xrange=[0.003, 10.0], $
      xstyle=1,$
      xtitle="Normal Velocity [cm/sec]", $
      ytitle = "Coefficent of Restitution (!Me!3!DN!N)", $
      yrange=[0, 1.1], $
      ystyle=1,$
      _EXTRA=_EXTRA

; Supulver, Bridge, eps=0.1, v*=0.01 cm/sec, v*=0.001 cm/sec
minx = [0.07000000, 0.04000000, 0.03000000, 0.05000000, 0.03000000]
maxx = [0.3400000, 0.1800000, 0.1400000, 0.1800000, 0.1500000]

for i=n_elements(powers)-1, 0, -1 do begin
   power = powers[i]
   A = vbs[i]^(-power)

   eps = A*v^(power)
   bad = where(eps GT 1)
   if(bad[0] ne -1) then eps[bad] = 1.0
   
   oplot, v, eps, linestyle=0, color=colors[linen], thick=2, _EXTRA=_EXTRA

   peak = where((v gt minx[i]) and (v lt maxx[i]))
   if(peak[0] ne -1) then begin
       oplot, v[peak], eps[peak], linestyle = 0, color=colors[linen], thick=5, _EXTRA=_EXTRA
   endif
   linen = linen-1
endfor
linen = linen + 3


for i=0, n_elements(vstar)-1 do begin
   x = vstar[i]/v
   eps = sqrt(-2.0/3.0 * x^2 + sqrt(10.0/3.0 * x^2 - 5.0/9.0 * x^4))
   bad = where(v lt vstar[i])
   if(bad[0] ne -1) then eps[bad] = 1.0
   oplot, v, eps, linestyle = 0, color=colors[linen], thick=2, _EXTRA=_EXTRA
   peak = where((v gt minx[i+3]) and (v lt maxx[i+3]))
   if(peak[0] ne -1) then begin
       oplot, v[peak], eps[peak], linestyle = 0, color=colors[linen], thick=5, _EXTRA=_EXTRA
   endif
   linen = linen+1
endfor




;powers_str = num2str(powers, format="(F5.2)")
powers_str = string(powers, format="(F5.2)")
powers_str = ['Supulver et al. (1995)', 'Bridges et al. (1984)']

;legend, powers_str, $
;        linestyle=replicate(0, n_elements(powers_str)), $
;        color=colorstep*(indgen(n_elements(powers_str))+2),$
;        pos= [0.08, 0.9], $
;        _EXTRA=_EXTRA






;vstar_str = num2str(vstar, format="(F5.3)")
vstar_str = string(vstar, format="(F5.3)")
vstar_str = "Borderies et al. (1984),!C     v!E*!N = " + vstar_str + " cm/sec"

;const01 = replicate(0.1, n_elements(v))
;oplot, v, const01, linestyle=linen, thick=2, color=colors[linen]
;peak = where((v gt minx[2]) and (v lt maxx[2]))
;if(peak[0] ne -1) then begin
;    oplot, v[peak], const01[peak], linestyle =linen, color=colors[linen], thick=5, _EXTRA=_EXTRA
;endif
;linen++




;for i=0, n_elements(gstars)-1 do begin
;   x = v/gstars[i]
;   eps = (1.0+2.583*x^(0.2))/(1 + 3.583*x^0.2 + 2.983*x^0.4 + 1.148*x^0.6 + 0.326*x^0.8)
;   bad = where(v lt vstar[i])
;   if(bad[0] ne -1) then eps[bad] = 1.0
;   oplot, v, eps, color = colorstep*(n_elements(gs)+8+i), _EXTRA=_EXTRA
;
;endfor
;gstar_str = num2str(gstars, format="(F4.2)")
;gstar_str = "g!E*!N = " + gstar_str + " cm/sec"

;;legend, [powers_str, vstar_str, '!7e!3!DN!N = 0.1'], $
;legend, [powers_str, vstar_str], $
;        linestyle=indgen(linen), $
;        color=colors, $
;;        pos= [0.15, 1.05], $
;        pos= [0.5, 1.0], $
;        charsize = 0.8, $
;        pspacing=2, $
;        spacing=1, $
;        _EXTRA=_EXTRA

for j=0,linen-1 do begin
  ;oplot, [0.3,0.8], (1.01-0.1*j)*[1,1], l=j
  xyouts, .07, 1.03-0.05*j, ([powers_str,vstar_str])[j], chars=0.8, co=colors[j]
endfor

if keyword_set(dolzr) then clzr

end
