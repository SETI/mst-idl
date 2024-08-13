q=findgen(601)/100
m=2.84e-3 
rho=1
z=30e5
x=30e5
rmax=100e2
rmin=1.
plot, q, 3*(4-q)/4/(3-q)*m*z/rho/x*$
      (rmax^(3-q)-rmin^(3-q))/(rmax^(4-q)-rmin^(4-q)), $
      /ylog, xtit='q', ytit='tau'
rmin=0.5
oplot, q, 3*(4-q)/4/(3-q)*m*z/rho/x*$
       (rmax^(3-q)-rmin^(3-q))/(rmax^(4-q)-rmin^(4-q))
rmin=2.
oplot, q, 3*(4-q)/4/(3-q)*m*z/rho/x*$
       (rmax^(3-q)-rmin^(3-q))/(rmax^(4-q)-rmin^(4-q))
rmin=1.
rmax=50e2
oplot, q, 3*(4-q)/4/(3-q)*m*z/rho/x*$
       (rmax^(3-q)-rmin^(3-q))/(rmax^(4-q)-rmin^(4-q))
rmax=200e2
oplot, q, 3*(4-q)/4/(3-q)*m*z/rho/x*$
       (rmax^(3-q)-rmin^(3-q))/(rmax^(4-q)-rmin^(4-q))
xyouts, align=1, !x.crange[1] - (!x.crange[1]-!x.crange[0])*.05, $
        10^(!y.crange[1] - (!y.crange[1]-!y.crange[0])*.22), 'rmin = 2 cm'
xyouts, align=1, !x.crange[1] - (!x.crange[1]-!x.crange[0])*.05, $
        10^(!y.crange[1] - (!y.crange[1]-!y.crange[0])*.16), 'rmin = 1 cm'
xyouts, align=1, !x.crange[1] - (!x.crange[1]-!x.crange[0])*.05, $
        10^(!y.crange[1] - (!y.crange[1]-!y.crange[0])*.1), 'rmin = 0.5 cm'
xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.05, $
        10^(!y.crange[0] + (!y.crange[1]-!y.crange[0])*.06), 'rmax = 200 m'
xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.05, $
        10^(!y.crange[0] + (!y.crange[1]-!y.crange[0])*.12), 'rmax = 100 m'
xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.05, $
        10^(!y.crange[0] + (!y.crange[1]-!y.crange[0])*.18), 'rmax = 50 m'

oplot, !x.crange, [1e-6,1e-6], l=1

stop

qq=2
r0=1.
rmax=50e2
plot, [1,3e5], [.01,1e4], /xs, /ys, /xlog, /ylog, /nodata, $
      xtit='r (cm)', ytit='N'
oplot, 10^!x.crange, [1,1], l=1
for qq2=2,6 do begin
  qq = qq2*.5+.1
  a = 3*(4-qq)*m/4/!pi/x/r0^qq/(rmax^(4-qq)-rmin^(4-qq))
  r = 2*10^(findgen(51)/10)
  n = a*r0^qq/(qq-1)*r^(-qq+1)
  vol = 2*!pi*2.5*764e5*30e5*30e5
  oplot, r, n*vol
  xyouts, 2, .1, 'rmin = '+strtrim(rmin,2)+' cm'
  xyouts, 2, .05, 'rmax = '+strtrim(rmax,2)+' cm'
endfor

end
