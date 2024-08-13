; Calculations based on the theoretical work of Crida et al (2010, AJ)
; to update the theory of Type I migration for the case of Saturn's rings.

; Newton's constant in cm^3 g^{-1} s^{-2}
cap_g = 6.674e-8
; Mass of Saturn, in g
cap_m = 5.6846e29
; Approximate mass of Bleriot, in grams = (4/3)*!pi*(1e5)^3 (assume 1 km radius)
; extra factor to make my curve (solid) align with Crida's (green dashed)
mm = 4.2e15
if keyword_set(usefudge) then mm = mm*500
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
indpos = findgen(num/2)+num/2
bb1 = bb[indpos]

; Trace DeltaJ_num curve (red) in Figure 3 of Crida10
; Curve follows through these points, linear on log-log plot:
points1 = [ [.5,.5*rh/rm], [1.5,1.5*rh/rm], [1.774,.0003], $
           [1.774,1.774*rh/2/rm], [2.5,2.5*rh/2/rm], [2.5,.00022], [2.7,3e-5], $
           [3.2,1e-5], [5,5e-7], [10,1.5e-8] ]
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

if keyword_set(plot1) then begin
  plot, [0.5,10], [1e-10,.001], /nodata, /xlog, /ylog, /xs, /ys, $
        xtit='b/r!DH!N', ytit='!MDJ [r!Dm!N!U2!N!Mw]'
  oplot, bb1/rh, deltaj_num, co=ctred()
endif

rr1 = rm + bb1
cap_omega1 = sqrt(caviar_omega2(rr1/1e5))

toversigma = int_tabulated( bb1, deltaj_num * rr1 * abs(omega-cap_omega1) * $
                                 rm^2 * omega )/2
print, 'T/sigma = ' + strtrim(toversigma,2) + ' cm^4/s^2'

end
