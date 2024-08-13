function make_radi, mnrad, mxrad, mnlon, mxlon, rpj_sz, loni=loni, dradi=dradi, dloni=dloni

if n_params() eq 0 then begin
  print, 'Syntax:  Result = MAKE_RADI( mnrad, mxrad, mnlon, mxlon, rpj_sz, loni=loni )'
  retall
endif

ls=rpj_sz[1]
rs=rpj_sz[2]
if keyword_set(mnrad) and keyword_set(mxrad) then begin
  mnrad=mnrad[0] & mxrad=mxrad[0]
  radi=dindgen(rs)/(rs-1.)*(mxrad-mnrad)+mnrad
  dradi = (mxrad-mnrad)/(rs-1.)
endif else radi = -1
if keyword_set(mnlon) and keyword_set(mxlon) then begin
  mnlon=mnlon[0] & mxlon=mxlon[0]
  loni=dindgen(ls)/(ls-1.)*(mxlon-mnlon)+mnlon
  dloni = (mxlon-mnlon)/(ls-1.)
endif

return, radi

end
