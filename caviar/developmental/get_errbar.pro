function get_errbar, errbar, radscan_np, val, noerrbar, sdmean

if keyword_set(errbar) then _errbar = errbar else _errbar = val*0
if n_elements(_errbar) ne n_elements(val) or keyword_set(noerrbar) then begin
  _errbar = val*0
endif
if not keyword_exists(sdmean) then sdmean = 1
if keyword_set(radscan_np) and keyword_set(sdmean) then begin
  if n_elements(radscan_np) eq n_elements(_errbar) then begin
    _errbar = _errbar / sqrt(radscan_np); * sqrt(2)
  endif else print, $
           'No standard deviation of the mean because arrays don''t agree.'
endif

return, _errbar

end
