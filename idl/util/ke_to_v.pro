function ke_to_v,m,ke,float=float

; For given mass and relativistic kinetic energy, returns velocity.

; Total energy is given by m*gamma, for rest mass m and gamma=(1-v^2)^(-1/2).
; To get kinetic energy, subtract the rest mass from the total energy.
; To get velocity, simply solve the equation for v.

if n_params() eq 0 then begin
  print, 'Syntax:  Result = ke_to_v(m,ke)'
  print, 'v in km/s, m in amu, ke in eV.'
  return, -1
endif

m_ev = m * 938.272d6	; convert from amu to eV
c = 2.9979d5		; km/s

if keyword_set(float) then begin
  return, float(c*sqrt(1 - 1/(ke/m_ev + 1)^2))
endif else begin
  return, c*sqrt(1 - 1/(ke/m_ev + 1)^2)
endelse

end