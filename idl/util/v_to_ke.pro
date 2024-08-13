function v_to_ke,m,v

; For given mass and velocity, returns relativistic kinetic energy.

; Total energy is given by m*gamma, for rest mass m and gamma=(1-v^2)^(-1/2).
; To get kinetic energy, subtract the rest mass from the total energy.

if n_params() eq 0 then begin
  print, 'Syntax:  Result = v_to_ke(m,v)'
  print, 'v in km/s, m in amu, ke in eV.'
  return, -1
endif

m_ev = m * 938.272d6	; convert from amu to eV
c = 2.9979d5		; km/s

return, m_ev*( 1/sqrt(1-(v/c)^2) - 1 )

end