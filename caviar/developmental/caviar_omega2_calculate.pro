; P_2(0) = -1/2; P_4(0) = 3/8; P_6(0) = -5/16; P_8(0) = 35/128
; P''_2(0) = 3; P''_4(0) = -15/2; P''_6(0) = 105/8; P''_8(0) = -315/16
; P''''_2(0) = 0; P''''_4(0) = 105;  P''''_6(0) = -945/2; P''''_8(0) = 10395/8
pp = [ -1./2, 3./8, -5./16, 35./128 ]
pp2 = [ 3., -15./2, 105./8, -315./16 ]
pp4 = [ 0., 105., -945./2, 10395./8 ]
ii = [2.,4,6,8]
dividers = '     --------     --------     --------     --------'
derivcoeffs = [-5,-7,-9,-11]
var = [ 'omega', 'kappa', 'nu', 'eta', 'chi', 'beta', 'delta', 'lambda' ]
nvar = n_elements(var)
coeffs = fltarr(4,nvar)

coeffs = [ $
; omega^2 = gm/r^3 * ( 1 - Sum_i (i+1)*P_i(0)*J_i*(prad/r)^i )
           [-(ii+1)*pp], $
; kappa^2 = gm/r^3 * ( 1 + Sum_i (i+1)*(i-1)*P_i(0)*J_i*(prad/r)^i )
           [(ii+1)*(ii-1)*pp], $
; nu^2 = gm/r^3 * ( 1 - Sum_i J_i*(prad/r)^i*[ (i+1)*P_i(0) - P''_i(0) ] )
           [-( (ii+1)*pp - pp2 )], $
; eta^2 = gm/r^3 * ( 1 + Sum_i (i+1)*(i-1)*(i/2+3)/3*P_i(0)*J_i*(prad/r)^i )
           [(ii+1)*(ii-1)*(ii/2.+3)/3.*pp], $
; chi^2 = gm/r^3 * ( 1 - Sum_i (i+3)/3*J_i*(prad/r)^i
;                                *[ (i+1)*P_i(0) - P''_i(0) ] )
           [-(ii+3)/3.*( (ii+1)*pp - pp2 )], $
; beta^2 = gm/r^3 * ( 1 - Sum_i (i+4)*(i+3)*(i+2)*(i+1)/24
;                                 *P_i(0)*J_i*(prad/r)^i )
           [-(ii+4)*(ii+3)*(ii+2)*(ii+1)/24.*pp], $
; delta^2 = gm/r^3 * ( 1 - Sum_i (i+3)*(i+4)/12*J_i*(prad/r)^i
;                                  *[ (i+1)*P_i(0) - P''_i(0) ] )
           [-(ii+3)*(ii+4)/12.*( (ii+1)*pp - pp2 )], $
; lambda^2 = gm/r^3 * ( 1 - Sum_i J_i*(prad/r)^i*[ (i+1)*(i+3)/3*P_i(0) 
;                                                  - 2*(i+3)/3*P''_i(0)
;                                                  + 1/9*P''''_i(0) ] )
           [-( (ii+1)*(ii+3)/3.*pp - 2*(ii+3)/3.*pp2 + 1/9.*pp4 )] ]

denoms = [ [2.,8,16,128], $   ; omega
           [2.,8,16,128], $   ; kappa
           [2.,8,16,128], $   ; nu
           [1.,8,8,128], $    ; eta
           [2.,8,16,128], $   ; chi
           [2.,4,8,128], $    ; beta
           [4.,4,32,128], $   ; delta
           [2.,24,16,128] ]   ; lambda

for j=0,nvar-1 do begin
  print, var[j]+' coefficients:'
  print, coeffs[*,j]*denoms[*,j], fo='(F13.1,F13.1,F13.1,F13.1)'
  print, dividers
  print, denoms[*,j], fo='(F13.1,F13.1,F13.1,F13.1)'
  print, ''
  print, 'd'+var[j]+'2_dr coefficients:'
  print, coeffs[*,j]*denoms[*,j]*derivcoeffs, fo='(F13.1,F13.1,F13.1,F13.1)'
  print, dividers
  print, denoms[*,j], fo='(F13.1,F13.1,F13.1,F13.1)'
  print, ''
endfor

end
