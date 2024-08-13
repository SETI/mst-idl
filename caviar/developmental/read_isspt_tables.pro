;function read_isspt_tables, incid_ang, emiss_ang, phase_ang, rad

restore, '/home/borogove/iss/ioverf2tau/isspt_tables/isspt_tables.sav'
if not keyword_set(incid_ang) then incid_ang = 77.
if not keyword_set(emiss_ang) then emiss_ang = 90.
if not keyword_set(phase_ang) then phase_ang = 100.

ii = interpol( indgen(n_i), ivals, incid_ang )
ee = interpol( indgen(n_e), evals, emiss_ang )
pp = interpol( indgen(n_p), pvals, phase_ang )
check = interpolate( isdata, ii, ee )
if check ne n_r then begin
  print, 'Incidence angle or emission angle is outside accepted range.'
  return, -1
endif

end
