function k_to_sigma, mm, rres, oldstyle=oldstyle, kfit=kfit, sigma=sigma, $
                     mp93=mp93

; In a linear fit to wavenumber as a function of radius, 
; kfit[1] = k / (r-rres)
; In a quadratic fit to phase as a function of radius, 
; kfit[2] = phase / (r-rres)^2 = k / 2 / (r-rres)
; Note that I generally express phase in degrees, which must be converted here.

; The equation that I have previously used comes from Eq 13 of Rosen et al 1991a
; k = ( 2 pi (m-1) ) / ( 3.08 Rres^4 sigma_b ) * ( r - rres )
; Note that, in this equation, Rres is a ratio of Saturn's radius, whereas
; in my code rres is generally expressed in km.  r_S = 60330 km by convention

; However, I have found that this equation introduces systematic errors of ~1%,
; as is confirmed by comparing the results of this routine with and without
; the /oldstyle keyword.  The more exact equation is Rosen's Eq 8:
; k = scripty_D / ( 2 pi G rres sigma_b ) * ( r - rres )
; Here, rres is in km as usual, not in Saturn radii.  
; Rosen cites Cuzzi 1984 in giving scripty_D for the m=1 and m>1 cases.

if n_params() eq 0 then begin
  print, ''
  print, 'Syntax:  Result = K_TO_SIGMA( mm, rres )'
  print, ''
  print, 'Computes the constant of proportionality between wavenumber k and (r-rres)/sigma'
  print, 'If optional input keyword kfit is supplied, then output keyword sigma is computed.'
  print, ''
  print, 'In my code, any occurrences of:'
  print, ''
  print, '2*!pi/3.08/rres^4*60330.^4*(mm-1)'
  print, ''
  print, 'should be replaced with:'
  print, ''
  print, 'k_to_sigma(mm,rres)'
  print, ''
  print, 'Default uses a better approximation than that including a 3.08, or can set keyword oldstyle.'
  print, ''
  retall
endif

if not keyword_exists(mp93) then mp93 = 1

rsat = 60330.
cap_g = 6.672d-8  ; cm^3 / g / s^2
if keyword_set(oldstyle) then begin
  k_to_sigma = 2 * !pi * (mm-1) / ( 3.08 * rres^4 / rsat^4 )
endif else begin
  if mm eq 1 then begin
    saturn_constants, j2=j2
    scripty_d = 21. / 2 * j2 * (rsat/rres)^2 * caviar_omega2(rres)
  endif else if mm ge 2 then begin
    scripty_d = 3 * (mm-1) * caviar_omega2(rres)
  endif else stop, 'KFIT_TO_SIGMA:  mm must be a positive integer.'
  if keyword_set(mp93) then begin
    ; Expression from Marley and Porco 1993
    saturn_constants, j2=j2
    scripty_d = ( 3 - 4.5*j2*(rsat/rres)^2 ) * (mm-1) * caviar_omega2(rres) + $
                21. / 2 * j2 * (rsat/rres)^2 * caviar_omega2(rres)
  endif
  k_to_sigma = scripty_d / (2*!pi*cap_g*rres/1e5)
endelse

if keyword_set(kfit) then begin
  if n_elements(kfit) eq 2 then begin
    return, k_to_sigma / kfit[1]
  endif else if n_elements(kfit) eq 3 then begin
    return, k_to_sigma / (kfit[2]*2*!pi/180)
  endif else stop, 'KFIT_TO_SIGMA:  kfit must be the coefficients of either '+$
                   'a linear or a quadratic fit.'
endif

return, k_to_sigma

end
