function get_thoukm, radi, tkmbase

if n_params() eq 0 then begin
  print, 'Result = GET_THOUKM( radi )'
  return, -1
endif

if not keyword_set(tkmbase) then begin
  if max(radi)-min(radi) gt 1e4 then begin
    tkmbase = 100000
  endif else if max(radi)-min(radi) gt 1.3e3 then begin
    tkmbase=10000
  endif else tkmbase = 1000
endif
thoukm = min(radi) - (min(radi) mod tkmbase)

return, thoukm

end
