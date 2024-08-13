function resloc_omega2, r
  common resloc1, gm, prad, j2, j4, j6, omegasat
  return, gm / r^3 * ( 1 + j2*3/2*(prad/r)^2 - j4*15/8*(prad/r)^4 + j6*35/16*(prad/r)^6 )
end

function synodic_period, rr, periods=periods, days=days, years=years

if n_params() eq 0 then begin
  print, 'Syntax:  Result = SYNODIC_PERIOD( rr, periods= )'
  print, 'Assumes orbit about Saturn.  Input radii should have 2 elements and be in km.'
  print, 'Output synodic_period (as well as periods) are in seconds, unless set keyword days or years.'
  print, 'Gives synodic period of rr[1,*] wrt rr[0,*].  That is, Result>0 iff rr[1,*]<rr[0,*].'
  retall;
endif

common resloc1, gm, prad, j2, j4, j6, omegasat
;; Saturn mass, radius, and harmonics from JPL kernel cpck05May2004.tpc
;gm = 37931289.4836 ;km^3/s^2
;prad = 60330.0d0 ;km
;j2 = 0.016292243237d
;j4 = -0.000928716077d
;j6 = 0.000088845313
saturn_constants, gm=gm, prad=prad, j2=j2, j4=j4, j6=j6

rr = float(rr)
omega = sqrt(resloc_omega2( rr ))
periods = 2*!dpi / omega
synodic_period = 2*!dpi / (omega[1,*]-omega[0,*])
if keyword_set(days) then begin
  periods = periods / 86400
  synodic_period = synodic_period / 86400
endif else if keyword_set(years) then begin
  periods = periods / 3.16e7
  synodic_period = synodic_period / 3.16e7
endif
if n_elements(synodic_period) eq 1 then synodic_period = synodic_period[0]
return, synodic_period

end
