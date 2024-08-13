; Calculate the function C(q) as defined by Equations 29 and 30
; of Rappaport et al. (2009, Icarus). 
function cofq, qq, prec=prec

if not keyword_exists(qq) then qq = 0.1d
if not keyword_set(prec) then prec = 0.001

maxu = 5000l;100l
du = 0.001
out = 0
outlast = -1
while abs(out-outlast) gt prec do begin
  outlast = out
  uu = dindgen(maxu/du)*du + du
  qq2 = qq^2 * sin(uu)^2/uu^2
  hh = ( 1 - sqrt(1-qq2) ) / qq2 / sqrt(1-qq2)
  out = 4.0d0/!dpi * int_tabulated( uu, sin(uu)^2/uu^2 * hh )
  print, qq, maxu, du, out, $
         fo='("qq=",F4.2,"      maxu=",I8,"      du=",F8.5,"      out=",F8.5)'
  maxu = maxu*2
  du = du/2
endwhile
; Note that, by L'Hopital's rule, H goes to 1/2 as qq2 goes to zero. 
; Also note (from Mathematica) that the integral of sin(u)^2/u^2 is
;    ( cos(2u) - 1 + 2u*S(2u) )/2u, where S(z) is the Sine Integral, or
;    the integral from 0 to z of sin(t)/t*dt.  Also, S(Infty) = pi/2.
;    (see http://mathworld.wolfram.com/SineIntegral.html)
; Thus, as q goes to zero, the integral becomes that of sin(u)^2/u^2/2, which 
;    goes to S(2u)/2 = pi/4, which leads to C(q) = 1.

return, out

end
