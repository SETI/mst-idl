; Calculations based on the theoretical work of Crida et al (2010, AJ)
; to update the theory of Type I migration for the case of Saturn's rings.

if keyword_set(doboth) then cridamass = 0
if keyword_set(doboth) then plot1 = 1
start:
; Newton's constant in cm^3 g^{-1} s^{-2}
cap_g = 6.674e-8
; Mass of Saturn, in g
cap_m = 5.6846e29
; Mass of Bleriot, as given by Crida10
mm1 = 3e-12*cap_m
; Mass of Bleriot, in grams = (4/3)*!pi*(1e5)^3 (assume 1 km radius)
mm2 = 4.2e15
; Change from 1 g/cm^3 to 0.5 g/cm^3
mm2 = mm2 / 2
if keyword_set(cridamass) then mm=mm1 else mm=mm2
; Semimajor axis of Bleriot, in cm
rm = 134912.0d5
; Hill radius of Bleriot
rh = rm * (mm/3/cap_m)^(1./3)
; Mean motion of Bleriot, in s^{-1}
omega = sqrt(caviar_omega2(rm/1e5))
; Impact parameter of ring particles, range from -10 to +10 Hill radii about rm
num = 200000
bb = ( findgen(num)/num*20 - 10.0*(num-1.0)/num )*rh
; Impact parameter, positive branch only
indpos = findgen(num/2)+num/2
bb1 = bb[indpos]

; Crida10 Eq 32
alpha = 0.75d0 + (6.0d0*beselk(2./3,1)+3.0d0*beselk(2./3,0)) / (4.0d0*beselk(2./3,0)+2.0d0*beselk(2./3,1))
deltaj32 = 64.0d0 * (cap_g*mm)^2 * rm / (243.0d0*omega^3*bb1^5) * (2.0d0*beselk(2./3,0)+beselk(2./3,1))^2 * (1+alpha*bb1/rm)

; Find ratio between bb*rh/rm and deltaj32 at outer transition point (bb/rh=2.5)
qq = 2.5*rh/rm / interpol( deltaj32/rm^2/omega, bb1/rh, 2.5 )
; Trace DeltaJ_num curve (red) in Figure 3 of Crida10
; Curve follows through these points, linear on log-log plot:
points1 = [ [.5,.5*rh/rm], [1.5,1.5*rh/rm], [1.774,1.774*rh*2/rm], $
            [1.774,1.774*rh/2/rm], [2.5,2.5*rh/2/rm], [2.5,2.5*rh/rm], $
            [2.7,3*interpol( deltaj32/rm^2/omega, bb1/rh, 2.7 )], $
            [3.2,2.25*interpol( deltaj32/rm^2/omega, bb1/rh, 3.2 )], $
            [5,1.05*interpol( deltaj32/rm^2/omega, bb1/rh, 5 )], $
            [10,interpol( deltaj32/rm^2/omega, bb1/rh, 10 )] ]
logpoints1 = alog10(points1)
deltaj_num = 10^interpol( logpoints1[1,*], logpoints1[0,*], alog10(bb1/rh) )
foo1 = max(where( bb1/rh lt 1.774 ))
foo2 = max(where( bb1/rh lt 2.5 ))
bb1 = [ bb1[0:foo1], 1.774*rh, 1.774*rh, bb1[foo1+1:foo2], $
        2.5*rh, 2.5*rh, bb1[foo2+1:num/2-1] ]
deltaj_num = [ deltaj_num[0:foo1], $
               reform(points1[1,where(points1[0,*] eq 1.774)]), $
               deltaj_num[foo1+1:foo2], $
               reform(points1[1,where(points1[0,*] eq 2.5)]), $
               deltaj_num[foo2+1:num/2-1] ]
bb1[foo1+2] = bb1[foo1+2]*1.000000001d0
bb1[foo2+4] = bb1[foo2+4]*1.000000001d0
num1 = n_elements(bb1)
dtorque1 = (rm+bb1) * deltaj_num*rm^2*omega * $
           abs( omega - sqrt(caviar_omega2((rm+bb1)/1e5)) )
dtorque_cum = dtorque1
for j=1l,num1-1 do dtorque_cum[j] = dtorque1[j] + dtorque_cum[j-1]
torquecen = interpol( findgen(num1), dtorque_cum, max(dtorque_cum)/2 )
gausscen = interpol( bb1, findgen(num1), torquecen )
if keyword_set(deltafunc) then begin
  gausssigma = 5e2
  gaussian1, bb1, [1.0,gausscen,gausssigma], deltaj_num
  dtorque2 = (rm+bb1) * deltaj_num*rm^2*omega * $
            abs( omega - sqrt(caviar_omega2((rm+bb1)/1e5)) )
  deltaj_num = tsum( bb1, dtorque1 ) / tsum( bb1, dtorque2 ) * deltaj_num
endif

if keyword_set(plot1) then begin

  if keyword_set(dolzr) then begin
    lzr, 'cridacalc3', /half
    @plot_prepare
    if keyword_set(asymptotes) then plot_color
    !p.multi = [0,2,2]
  endif

  ; Reproduce Figure 3 of Crida et al. (2010)
  if not ( keyword_set(doboth) and keyword_set(cridamass) ) then begin
    if keyword_set(deltafunc) then fac=10 else fac=1
    plot, [0.5,10], [1e-9,.0001]*fac, /nodata, /xlog, /ylog, /xs, /ys, $
          xtit='Normalized Impact Parameter b/r!DH!N', ytit='Normalized '+$
          'Angular Momentum!CTransfer !MDJ/(J!Dm!Nr!DH!N), km!U-1!N'
  endif 
  oplot, bb1/rh, deltaj_num/rh*1e5;, co=ctred()
  if keyword_set(asymptotes) then begin
    foo = where( bb1/rh lt 1.5 )
    oplot, bb1[foo]/rh, bb1[foo]/rm/rh*1e5, l=3, co=ctyellow()
    foo = where( bb1/rh lt 2.5 )
    oplot, bb1[foo]/rh, bb1[foo]/rm/2/rh*1e5, l=4, co=ctyellow()
    foo = where( bb1/rh gt 3 )
    oplot, bb1[foo]/rh, deltaj32[foo]/rm^2/omega/rh*1e5, l=2, co=ctgreen()
  endif 

  for jj=-1,1,2 do begin
    if jj eq -1 then text='negative' else text='positive'
    rr1 = rm + jj*bb1
    cap_omega1 = sqrt(caviar_omega2(rr1/1e5))
    toversigma = tsum( bb1, deltaj_num * rr1 * $
                       abs(omega-cap_omega1) * rm^2 * omega )
    print, 'T/sigma ('+text+' branch) = ' + strtrim(toversigma,2) + ' cm^4/s^2'
  endfor

  if keyword_set(doboth) then begin
    if not keyword_set(cridamass) then begin
      points1a = points1
      cridamass = 1
      goto, start
    endif else begin
      points1b = points1
    endelse 
;    plot, points1a[0,*], points1a[1,*], /xs, /ys, /xlog, /ylog, $
;          yr=[1e-9,.001], xtit='b/r!DH!N', ytit='!MDJ [r!Dm!N!U2!N!Mw]'
;    oplot, points1b[0,*], points1b[1,*], co=ctred()
  endif

  if keyword_set(dolzr) then clzr
  stop

endif 
; Note: Figure 4 of Crida et al. (2010) is not relevant to my current effort
; because it hangs on small_deltaj_num for the case of constant surface density

; Now on to the numerical model...

; Surface density profile, first row is sigma, second row is radius
rcen = rm
omegacen = sqrt(caviar_omega2(rcen/1e5))
minsigma = 30.0d0
if keyword_set(deltafunc) then begin
  if not keyword_set(simple) then simple=0
  if simple eq 1 then begin
    if keyword_set(inward) then dsigma = 2.88d0 else dsigma = 0.876d0
    maxsigma = minsigma + dsigma
    sigmaprof = [ [maxsigma,maxsigma,minsigma,minsigma,maxsigma,maxsigma], $
                  [-100,-3.25,-2.5,2.5,3.25,100] ]
  endif else if simple eq 2 then begin
    dsigma = 3.84d0
    maxsigma = minsigma + dsigma
    sigmaprof = [ [maxsigma,maxsigma,minsigma,minsigma,maxsigma,maxsigma], $
                  [-100,-gausscen/1e5-0.5,-gausscen/1e5,gausscen/1e5,$
                   gausscen/1e5+1.64,100] ]
    ; With above sigmaprof, equilibrium semimajor axis is actually +244.0
    sigmaprof[*,1] = sigmaprof[*,1] - .00244
  endif else begin
  endelse 
endif else begin
  if simple eq 1 then begin
    if keyword_set(inward) then dsigma = 11.5d0 else dsigma = 3.5d0
    maxsigma = minsigma + dsigma
    sigmaprof = [ [maxsigma,maxsigma,minsigma,minsigma,maxsigma,maxsigma], $
                  [-100,-4.0,-3.5,3.5,4.0,100] ]
  endif else if simple eq 2 then begin
    if keyword_set(inward) then dsigma = 11.5d0 else dsigma = 3.5d0
    maxsigma = minsigma + dsigma
    sigmaprof = [ [maxsigma,maxsigma,minsigma,minsigma,maxsigma,maxsigma], $
                  [-100,-4,-1.5,1.5,4,100] ]
  endif else if simple eq 3 then begin
    if keyword_set(inward) then dsigma = 14.0d0 else dsigma = 4.5d0
    maxsigma = minsigma + dsigma
    sigmaprof = [ [maxsigma,maxsigma,minsigma,minsigma,maxsigma,maxsigma], $
                  [-100,-2.75,-2.75,2.75,2.75,100] ]
  endif else if simple eq 4 then begin
;minsigma = 0.0d0
    dsigma = 5.0d0
    maxsigma1 = minsigma + dsigma
    maxsigma2 = minsigma + dsigma*0.3
    medsigma = minsigma
    minsigma1 = medsigma - dsigma
    minsigma2 = medsigma - dsigma*0.3
    sigmaprof = [ [medsigma,medsigma,maxsigma1,minsigma1,medsigma,$
                   medsigma,maxsigma1,minsigma1,medsigma,$
                   medsigma,minsigma2,maxsigma2,medsigma,$
                   medsigma,minsigma2,maxsigma2,medsigma,medsigma], $
                  [-100,-2.5*rh/1e5-.5,-2.5*rh/1e5-.5,-2.5*rh/1e5,-2.5*rh/1e5,$
                   -1.774*rh/1e5-.5,-1.774*rh/1e5-.5,-1.774*rh/1e5,$
                   -1.774*rh/1e5,1.774*rh/1e5-.5,1.774*rh/1e5-.5,$
                   1.774*rh/1e5,1.774*rh/1e5,2.5*rh/1e5-.5,2.5*rh/1e5-.5,$
                   2.5*rh/1e5,2.5*rh/1e5,100] ]
  endif else if simple eq 0 then begin
    dsigma = 11.5d0
    maxsigma = minsigma + dsigma
    medsigma = minsigma + dsigma*.4
    sigmaprof = [ [maxsigma,maxsigma,medsigma,minsigma,minsigma,maxsigma,maxsigma], $
                  [-100,-4.25,-3.9,-1,1,7,100] ]
    ; With above sigmaprof, equilibrium semimajor axis is actually -19149.8
    sigmaprof[*,1] = sigmaprof[*,1] + .05869
  endif 
endelse
sigmaprof[*,1] = sigmaprof[*,1]*1e5 + rcen
dsigmadr = [ (sigmaprof[1,0]-sigmaprof[2,0])/(sigmaprof[2,1]-sigmaprof[1,1]), $
             (sigmaprof[4,0]-sigmaprof[3,0])/(sigmaprof[4,1]-sigmaprof[3,1]) ]
; Time
if keyword_set(inward) then dt = 3.16e5 else dt = 3.16e5*3.6 ; seconds
nt = 101
ttt = findgen(nt)*dt
; Angular momentum exchange
bb2 = [ -reverse(bb1),  bb1 ]
deltaj_num2 = [ reverse(deltaj_num), -deltaj_num ]
num2 = n_elements(bb2)
; Arrays to be filled
rm = dblarr(nt)
if keyword_set(inward) then rm[0] = rcen - 29893 else rm[0] = rcen + 12842
torque = dblarr(nt)
lon = dblarr(nt)
sigma = dblarr(num2)
if keyword_set(dolzr) then begin
  psn = 'cridacalc_movie'
  if keyword_set(inward) then psn=psn+'_inward' else psn=psn+'_outward'
  if simple eq 1 then psn=psn+'_simple1'
  if simple eq 2 then psn=psn+'_simple2'
  lzr, psn, /half
  @plot_prepare
  plot_color
  chars = 1
  !x.margin = [0,10]
endif else begin
  chars = 3
  !x.margin = [10,10]
endelse
!p.multi = [0,2,2]
!p.charsize = 1
!y.margin = 0
!y.omargin = [4,2]
for j=0l,nt-1 do begin
  omega = sqrt(caviar_omega2(rm[j]/1e5))
  if j gt 0 then lon[j] = lon[j-1] + (omega-omegacen)*dt
  r0 = rm[j] + bb2
  cap_omega = sqrt(caviar_omega2(r0/1e5))
  sigma = interpol( sigmaprof[*,0], sigmaprof[*,1], r0 )
  ; From Crida10 Eq 36
  dtorque = sigma * r0 * deltaj_num2*rcen^2*omega * abs(omega-cap_omega)
  torque[j] = tsum( bb2, dtorque )
  if j eq 0 or j eq nt-1 then begin
    if j eq 0 then begin
      xtit = ''
      xtn = replicate(' ',20)
      lett = '(a)'
    endif else begin
      xtit = 'Impact Parameter b (km)'
      xtn = ''
      lett = '(b)'
    endelse 
    plot, (bb2+rm[j]-rcen)/1e5, sigma, xr=[-5,5], xtickn=xtn, xtit=xtit, $
          yr=[minsigma-dsigma*.1,maxsigma+dsigma*.1], $
          ytit='Surface Density (g cm!U-2!N)', /xs, ys=9
    axis, yaxis=1, yr=[0,max(dtorque)*1.1], /ys, /save, $
          ytit='Torque Integrand (g cm s!U-2!N)', co=ctred()
    oplot, (bb2+rm[j]-rcen)/1e5, abs(dtorque), co=ctred()
    oplot, (rm[[j,j]]-rcen)/1e5, !y.crange, l=1, co=ctred()
    axis, xaxis=0, /xs, xr=!x.crange, xtickn=notn
    xyouts, 0, !y.crange[1]-(!y.crange[1]-!y.crange[0])*.1, align=.5, $
            't = '+string(ttt[j]/3.16e7,fo='(F3.1)')+' yr', chars=chars
    xyouts, !x.crange[1]-(!x.crange[1]-!x.crange[0])*.05, align=.5, $
            !y.crange[0]+(!y.crange[1]-!y.crange[0])*.05, lett, chars=1
    if j eq 0 then !p.multi[0]=!p.multi[0]-1; else !p.multi[0]=!p.multi[0]+2
  endif 
  ; Since, in this case where e is negligible, the angular momentum is
  ; h = mm*sqrt(mu*a*(1-e^2)) = mm*sqrt(mu*a) = mm*n*a^2, we have 
  ; da/dt = 2*h/mu/mm * dh/dt = 2/n/a/mm * dh/dt = 2*sqrt(a/mu)/mm * dh/dt, 
  ; where of course dh/dt is the torque.
  drm = 2*sqrt(rm[j]/cap_g/cap_m)/mm*torque[j]*dt
  if j ne nt-1 then rm[j+1] = rm[j] + drm
endfor
notn = replicate(' ',20)
if keyword_set(dolzr) then !x.margin = [10,0] else !x.margin = [17,3]
!p.multi = [5,2,3]
!p.charsize = 2
plot, ttt/3.16e7, (rm-rcen)/1e5, xtickn=notn, /xs, /ys, $
      ytit='Radial Distance from!CEquilibrium (km)'
if not keyword_set(inward) then begin
  oplot, [1.3,1.3], [!y.crange[1],-!y.crange[1]*2], l=2, /noclip
endif
xyouts, !x.crange[1]-(!x.crange[1]-!x.crange[0])*.05, align=.5, $
        !y.crange[0]+(!y.crange[1]-!y.crange[0])*.05, '(c)', chars=1
!p.multi[0] = !p.multi[0]-1
plot, ttt/3.16e7, lon*rcen/1e5, xtickn=notn, /xs, /ys, $
      ytit='Azimuthal Residual (km)'
xyouts, !x.crange[1]-(!x.crange[1]-!x.crange[0])*.05, align=.5, $
        !y.crange[0]+(!y.crange[1]-!y.crange[0])*.05, '(d)', chars=1
!p.multi[0] = !p.multi[0]-1
plot, ttt/3.16e7, torque, xtit='Time (yr)', /xs, /ys, $
      ytit='Torque (g cm!U2!N s!U-2!N)'
xyouts, !x.crange[1]-(!x.crange[1]-!x.crange[0])*.05, align=.5, $
        !y.crange[0]+(!y.crange[1]-!y.crange[0])*.05, '(e)', chars=1
!x.margin = [10,3]
!y.margin = [4,2]
!y.omargin = 0

if keyword_set(dolzr) then clzr

end
