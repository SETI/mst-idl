    minmaxonly = 1
    get_radarray, cam_params, cmat, nl, et, polera, poledec, sc, radarray, lonarray, outofplane=outofplane, nointerp=nointerp, /minmaxonly, planet_coords=planet_coords
    if keyword_set(outofplane) then begin &$
      _rmin = 60330.0d & _rmax = 1d6 &$
      _lmin = 0.0d & _lmax = 360.0d &$
    endif else begin &$
      _rmin = min(radarray) & _rmax = max(radarray) &$
      _lmin = min(lonarray) & _lmax = max(lonarray) &$
      ;ll = lonarray[sort(lonarray)] 
      ;dll = ll[1:n_elements(ll)-1] - ll[0:n_elements(ll)-2] 
      ;if max(dll) gt 10 then begin ; This value is adjustable
      ;  _lmax = ll[where( dll eq max(dll) )]
      ;  _lmin = ll[where( dll eq max(dll) )+1] - 360
      ;endif else begin
      ;  _lmax = max(ll)
      ;  _lmin = min(ll)
      ;endelse

    endelse
    if not keyword_set(rlminmax_silent) then print, '_rmin = '+strtrim(_rmin,2)+'   _rmax = '+strtrim(_rmax,2)+'   _lmin = '+strtrim(_lmin,2)+'   _lmax = '+strtrim(_lmax,2)

;    end
