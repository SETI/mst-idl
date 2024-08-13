pp_l = 1.37d0          ; 2pi/omegap, in days, due to Enceladus mean anomaly (Table 1)
pp_omega2 = 4035.63d   ; 2pi/omegap, in days, due to Enceladus-Dione orbital libration (Table 1)
pp_phi4 = 1418.93d     ; 2pi/omegap, in days, due to proper pericenter of Dione (Table 1)
hh_l = 1954.84d        ; Longitude residual amplitude, in arcsec, due to Enceladus mean anomaly (Table 1)
hl_l = -103.73d        ; Physical libration amplitude, in arcsec, due to Enceladus mean anomaly (Table 2)
hh_omega2 = 933.43d    ; Longitude residual amplitude, in arcsec, due to Enceladus-Dione orbital libration (Table 1)
hh_phi4 = 676.54d      ; Longitude residual amplitude, in arcsec, due to proper pericenter of Dione (Table 1)
alpha_l = 27.83d       ; Longitude residual phase, in arcsec, due to Enceladus mean anomaly (Table 1)
alpha_omega2 = -83.26d ; Longitude residual phase, in arcsec, due to Enceladus-Dione orbital libration (Table 1)
alpha_phi4 = 128.68d   ; Longitude residual phase, in arcsec, due to proper pericenter of Dione (Table 1)
sigma = 0.0168d        ; (B-A)/C (text below Figure 1)
k2 = 0.013d            ; Love number (text below Equation 10)
rr = 252.1d            ; Enceladus radius, in km
cc = 0.35d             ; Enceladus dimensionless polar moment of inertia (estimated)
menc = 1.080d20        ; Enceladus mass, in kg
msat = 5.685d26        ; Saturn mass, in kg
cap_g = 6.674e-11      ; Newton's constant, in m^3/kg/s^2
nn = 2*!dpi/pp_l       ; Enceladus mean motion, 4.58 rad/d (per text above Table 2)
mm = 0.33d             ; Frequency-response exponent (Figure 1 and text below Equation 9)
beta = 1d-12           ; Friction parameter (Figure 1 and text below Equation 9)
etamin = 1d12          ; Minimum ice viscosity, in Pa s (Figure 1 and last pgh of Section 2.4)
etamax = 1d17          ; Maximum ice viscosity, in Pa s (Figure 1 and last pgh of Section 2.4)
jdprimemin = 1.0d0/(etamin*omegai) + beta*omegai^(-mm)*sin(mm*!dpi/2)*gamma(mm+1)  ; Equation 9
jdprimemax = 1.0d0/(etamax*omegai) + beta*omegai^(-mm)*sin(mm*!dpi/2)*gamma(mm+1)  ; Equation 9
jprime = 1.0d0/cap_g + beta*omegai^(-mm)*cos(mm*!dpi/2)*gamma(mm+1)          ; Equation 9
sin_epsiloni_min = jdprimemin/sqrt(jprime^2+jdprimemin^2)   ; Equation 8
sin_epsiloni_max = jdprimemax/sqrt(jprime^2+jdprimemax^2)   ; Equation 8
omega0 = nn*sqrt(3*sigma)   ; Natural frequency (text below Equation 3), 1.03 rad/d (per text above Table 2)
omegai = 2*!dpi/[pp_l,pp_omega2,pp_phi4]  ; Driving frequencies
epsiloni = dblarr(3)  ; Frequency-dependent tidal phase lag (Equation 8?)
epsiloni[0] = 37.5d*!dpi/180  ; Value per text below Equation 10
dti = epsiloni / omegai  ; Time delay (last line of Section 2.4)
lambdai = k2*3/2*rr^3/cc*nn^4/cap_g/menc*dti  ; Frequency-damping coefficient (Equation 5)
; Dissipative/non-dissipative amplitude ratio (Equation 10)
pp = 1 - 2*lambdai^2*omegai^4*(2*omega0^2-omegai^2)/(omega0^2-omegai^2)*omega0^4


end
