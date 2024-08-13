; Calculations based on the theoretical work of Crida et al (2010, AJ)
; to update the theory of Type I migration for the case of Saturn's rings.

; Newton's constant in cm^3 g^{-1} s^{-2}
cap_g = 6.674e-8
; Mass of Saturn, in g
cap_m = 5.6846e29
; Approximate mass of Bleriot, in grams = (4/3)*!pi*(1e5)^3 (assume 1 km radius)
; extra factor to make my curve (solid) align with Crida's (green dashed)
mm = 4.2e15;*500
; Semimajor axis of Bleriot, in cm
rm = 134912.0d5
; Hill radius of Bleriot
rh = rm * (mm/3/cap_m)^(1./3)
; Mean motion of Bleriot, in s^{-1}
omega = sqrt(caviar_omega2(rm/1e5))
; Semimajor axis of ring particles, range from -10 to +10 Hill radii about rm
num = 200000
r0 = ( findgen(num)/num*20 - 10.0*(num-1.0)/num )*rh + rm
; Mean motion of ring particles, in s^{-1}
cap_omega = sqrt(caviar_omega2(r0/1e5))
; Impact parameter
bb = abs(r0-rm)

; Crida10 Eq 31
xi0 = cap_omega*abs(r0-rm)/abs(omega-cap_omega)/sqrt(r0*rm)
deltaj = 2.0d0 * (cap_g*mm)^2 / (r0^3*rm*(omega-cap_omega)^3) * ( beselk(xi0,0)*(0.5d0-2*cap_omega/(cap_omega-omega)) + beselk(xi0,1)*xi0*(0.5d0+rm/(r0-rm)) )^2

; Crida10 Eq 32
alpha = 0.75d0 + (6.0d0*beselk(2./3,1)+3.0d0*beselk(2./3,0)) / (4.0d0*beselk(2./3,0)+2.0d0*beselk(2./3,1))
deltaj2 = 64.0d0 * (cap_g*mm)^2 * rm / (243.0d0*omega^3*bb^5) * (2.0d0*beselk(2./3,0)+beselk(2./3,1))^2 * (1+alpha*bb/rm)

; Plot DeltaJ, compare to green line in Crida10's Figure 3.
if not keyword_exists(plot1) then plot1 = 1
if keyword_set(plot1) then begin
  window
  plot, bb/rh, abs(deltaj)/rm^2/omega, /xlog, /ylog, $
        xr=[0.5,10], yr=[1e-10,.001], /xs, /ys, $
        xtit='b/r!DH!N', ytit='!MDJ [r!Dm!N!U2!N!Mw]'
  oplot, [1.1,10], [.001,1.5e-8], l=2, co=ctgreen()
endif

; Crida10 Eq 34
indpos = findgen(num/2)+num/2
indneg = reverse(findgen(num/2))
;small_deltaj = deltaj[indpos] - deltaj[indneg]
small_deltaj = 2*deltaj[indpos]*bb[indpos]/rm

bb1 = bb[indpos]
; Curve follows through these points, linear on log-log plot:
points1 = [ [.5,.5*rh/rm], [1.5,1.5*rh/rm], [1.774,.0003], $
           [1.774,1.774*rh/2/rm], [2.5,2.5*rh/2/rm], [2.5,.00022], [2.7,3e-5], $
           [3.2,1e-5], [5,5e-7], [10,1.5e-8] ]
logpoints1 = alog10(points1)
;points2 = [ [.5,3e-9], [1.3,2.2e-8], [1.774,4e-7], [1.774,8e-9], [2.5,1.5e-8],$
;           [2.5,1e-6], [2.7,1.2e-7], [3.2,2.2e-8], [5,1.2e-9], [9.2,1e-10] ]
points2 = [ [.5,3e-9], [1.3,1e-8], [1.774,2.3e-7], [1.774,8e-9], [2.5,1.5e-8], $
           [2.5,1e-6], [2.7,1.2e-7], [3.2,2.2e-8], [5,1.2e-9], [9.2,1e-10] ]
logpoints2 = alog10(points2)
deltaj_num = 10^interpol( logpoints1[1,*], logpoints1[0,*], alog10(bb1/rh) )
small_deltaj_num = 10^interpol( logpoints2[1,*], logpoints2[0,*], $
                                alog10(bb1/rh) )
foo1 = max(where( bb1/rh lt 1.774 ))
foo2 = max(where( bb1/rh lt 2.5 ))
bb1 = [ bb1[0:foo1], 1.774*rh, 1.774*rh, bb1[foo1+1:foo2], $
        2.5*rh, 2.5*rh, bb1[foo2+1:num/2-1] ]
deltaj_num = [ deltaj_num[0:foo1], $
               reform(points1[1,where(points1[0,*] eq 1.774)]), $
               deltaj_num[foo1+1:foo2], $
               reform(points1[1,where(points1[0,*] eq 2.5)]), $
               deltaj_num[foo2+1:num/2-1] ]
small_deltaj_num = [ small_deltaj_num[0:foo1], $
                     reform(points2[1,where(points2[0,*] eq 1.774)]), $
                     small_deltaj_num[foo1+1:foo2], $
                     reform(points2[1,where(points2[0,*] eq 2.5)]), $
                     small_deltaj_num[foo2+1:num/2-1] ]
bb1[foo1+2] = bb1[foo1+2]*1.000000001d0
bb1[foo2+4] = bb1[foo2+4]*1.000000001d0
if keyword_set(plot1) then begin
  oplot, bb1/rh, deltaj_num, l=2, co=ctred()
  oplot, bb1/rh, small_deltaj_num, l=2, co=ctblue()
endif

num1 = n_elements(bb1)
bb2 = [ -reverse(bb1), bb1 ]
num2 = n_elements(bb2)
cap_omega2 = sqrt(caviar_omega2((bb2+rm)/1e5))
deltaj_num2 = [ -reverse(deltaj_num), deltaj_num ]/2
small_deltaj_num2 = [ -reverse(small_deltaj_num), small_deltaj_num ]/2
sigma0 = 40.0d0   ;g/cm^2
dtorque_c = sigma0 * (rm+bb2) * small_deltaj_num2 * abs(omega-cap_omega2)
dtorque_c1 = dtorque_c[num2/2:num2-1] - reverse(dtorque_c[0:num2/2-1])
torque_c1 = dtorque_c1*0
;for j=1,num1-1 do torque_c1[j] = int_tabulated( bb1[0:j], dtorque_c1[0:j], $
;                                                /double )
for j=1l,num1-1 do torque_c1[j] = torque_c1[j-1]+dtorque_c1[j]*(bb1[j]-bb1[j-1])

if keyword_set(plot2) then begin
  window, 1
  plot, bb1/rh, torque_c1/max(torque_c1)*17.8, /ys, yticki=2, xtit='b/r!DH!N', ytit='Cumulative Torque'
endif

rcen = rm
omegacen = sqrt(caviar_omega2(rcen/1e5))
aa = .5e-10
exp = 2
;aa = 1e-35
;exp = 6
dt = 3e5;1000.0d0  ; seconds
nbbb = 100
bbb = findgen(nbbb)
ttt = findgen(nbbb)*dt
torque = fltarr(nbbb)
lon = fltarr(nbbb)
sigma = fltarr(num2)
if keyword_set(dolzr) then begin
  if dolzr eq 2 then begin
    lzr, 'cridacalc_movie000'
    ;@plot_prepare
    !p.thick = 5
    !x.thick = 5
    !y.thick = 5
    !p.font = 1
    plot_color
    bcen = 1e5  ; 2 km, in cm
  endif
endif else dolzr = 0
if dolzr ne 2 then begin
  bcen = 2e4  ; 200 meters, in cm
  window
endif
!p.multi = [0,2,2]
for j=0l,nbbb-1 do begin
  bbb[j] = -bcen
  rm2 = rcen - bcen
  rr2 = rm2 + bb2
  cap_omega2a = sqrt(caviar_omega2(rr2/1e5))
  omega2 = sqrt(caviar_omega2(rm2/1e5))
  if j eq 0 then lon[j] = 0 else lon[j] = lon[j-1] + (omega2-omegacen)*dt
  sigma = sigma0 + aa*(bb2-bcen)^exp
sigma[where( (bb2-bcen)/rh gt -1.5 and (bb2-bcen)/rh lt 1.5 )] = sigma[max(where( (bb2-bcen)/rh le -1.5 ))]
sigma[where( (bb2-bcen)/rh lt -3 )] = sigma[min(where( (bb2-bcen)/rh ge -3 ))]
sigma[where( (bb2-bcen)/rh gt 3 )] = sigma[min(where( (bb2-bcen)/rh ge -3 ))]
sigma = sigma - min(sigma) + 32
  if dolzr eq 2 then begin
    if j mod 5 eq 0 then begin
      if j ne 0 then lzr, 'cridacalc'+string(j,fo='(I03)')
      !p.multi = 0
      !p.charsize = 3
      plot, (bb2-bcen)/rh, sigma, xr=[-3.5,3.5], yr=[30,45], $
            xtit='b/r!Dh!N', ytit='Surface Density (g/cm!U2!N)', ys=9
      axis, yaxis=1, yr=[0,.4], /ys, /save, ytit='Torque Contribution (g cm!U2!N/s)', co=ctred()
      oplot, (bb2-bcen)/rh, abs(dtorque), co=ctred()
      xyouts, -3.7, .37, 't = '+string(ttt[j]/3.16e7,fo='(F4.2)')+' yr', chars=3
      clzr
      print, 'cridacalc'+string(j,fo='(I03)')
    endif
  endif else begin
    ;if j mod (nbbb/5) eq 0 and j ne 0 then print, 'j = '+strtrim(j,2)
    if j mod (nbbb/5) eq 0 and j ne 0 then begin
      plot, (bb2-bcen)/rh, sigma, xr=[-3.5,3.5], yr=[30,45], $
            xtit='b/r!Dh!N', ytit='Surface Density (g/cm!U2!N)', ys=9
      axis, yaxis=1, yr=[0,.4], /ys, /save, ytit='Torque Contribution (g cm!U2!N/s)', co=ctred()
      oplot, (bb2-bcen)/rh, abs(dtorque), co=ctred()
    endif
  endelse
  ;; Make sigma equal to sigma0 at 1.5*rh and sigma0+10 and 3*rh
  ;sigma = 10/1.5/rh*(abs(bb2)-1.5*rh) + sigma0
  ; Crida10 Eq 36
  dtorque = sigma * deltaj_num2 * rr2 * abs(omega2-cap_omega2a)
  ;dtorque = sigma * deltaj_num2 * rm2 * abs(omega2-cap_omega2a)
  torque[j] = int_tabulated( bb2, dtorque, /double )
  ; Since the angular momentum h = sqrt(mu*a*(1-e^2)) = sqrt(mu*a) in
  ; this case where e is negligible, we have dt/dt = 2*h/mu * dh/dt, 
  ; where of course dh/dt is the torque.  We can calculate h with a=rcen,
  ; neglecting the small changes in a. 
  bcen = bcen - 2*sqrt(rcen/cap_g/cap_m)*torque[j]*dt
endfor
if dolzr eq 1 then begin
  lzr, 'cridacalc', /half
  @plot_prepare
endif else window, 1
!p.multi = [0,2,3]
!y.omargin = [4,2]
!y.margin = 0
!p.charsize = 2
notn = replicate(' ',20)
plot, ttt/max(ttt), bbb/1e5, xtickn=notn, ytit='Radial Distance from!CDensity Minimum (km)'
!p.multi[0] = !p.multi[0] - 1
plot, ttt/max(ttt), torque, xtickn=notn, ytit='Torque (g cm!U2!N/s!U2!N)'
!p.multi[0] = !p.multi[0] - 1
plot, ttt/max(ttt), lon*rcen/1e5, xtit='Time (yr)', ytit='Azimuthal Residual (km)'
!y.omargin = 0
!y.margin = [4,2]
!p.charsize = 0
if dolzr eq 1 then clzr

end
