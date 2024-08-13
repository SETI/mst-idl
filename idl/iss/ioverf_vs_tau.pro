function ioverf_vs_tau, incidence_angle, emission_angle, phase_angle, $
  albedo, normalize=normalize, dolzr=dolzr, psname=psname, debug=debug, $
  transmission=transmission, tau1=tau1, notaumax=notaumax, _tau=tau, $
  taumax=taumax

if n_params() eq 0 then begin
  print, 'Syntax:  Result = IOVERF_TO_TAU( incidence_angle, emission_angle, phase_angle, albedo )'
  retall
endif

if not keyword_set(phase_angle) and not keyword_set(albedo) then normalize = 1

if keyword_set(phase_angle) then begin
  print, 'Phase function not implemented.  Using P(alpha) = 1.'
  pp = 1
endif else pp = 1

if keyword_set(albedo) then begin
  print, 'Albedo not implemented.  Using albedo = 1.'
  albedo = 1
endif else albedo = 1

if not keyword_exists(transmission) then begin
  if incidence_angle gt 90 and emission_angle gt 90 then begin
    transmission = 0
  endif else if incidence_angle lt 90 and emission_angle lt 90 then begin
    transmission = 0
  endif else transmission = 1
endif
if incidence_angle gt 90 then incidence_angle = 180 - incidence_angle
if emission_angle gt 90 then emission_angle = 180 - emission_angle
mu = cos( incidence_angle * !dpi / 180 )
muprime = cos( emission_angle * !dpi / 180 )

if not keyword_set(ntau) then ntau = 2000
if not keyword_set(tau1) then tau1 = 2
tau = (findgen(ntau+1)+1) / ntau * tau1
if transmission eq 0 then begin
  ioverf = 1 - exp( -tau * (mu+muprime) / mu / muprime )
endif else begin
  ioverf = exp(-tau/muprime) - exp(-tau/mu)
endelse

taumax = interpol( tau, deriv(ioverf), 0 )
if keyword_set(verbose) then print, 'Taumax = '+strtrim(taumax,2)
if keyword_set(normalize) then begin
  ytit = 'Relative I/F'
  ioverf = ioverf / max(ioverf)
endif else ytit = 'I/F'

if keyword_set(dolzr) then begin
  if not keyword_set(psname) then psname = 'plot'
  lzr, psname
  @plot_prepare
endif

plot, tau, ioverf, xtit='Normal Optical Depth', ytit=ytit
if not keyword_set(notaumax) then oplot, [1,1]*taumax, !y.crange, l=1

if keyword_set(dolzr) then clzr
if keyword_set(debug) then stop

return, ioverf

end
