success = 0
if keyword_set(lmin) and keyword_set(lmax) and keyword_exists(jimg) then if jimg ne -1 then begin &$
  dl = lmax[jimg] - lmin[jimg] &$
  dl = dl < 5 &$
  startlon = lmin[jimg] - dl &$
  stoplon = lmax[jimg] + dl &$
  success = 1 &$
endif
if success eq 0 then begin &$
  startlon = 0.0d0 &$
  stoplon = 360.0d0 &$
endif

;end
