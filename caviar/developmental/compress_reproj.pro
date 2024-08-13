function compress_reproj, rpi, _xfac, _yfac, xlen=xlen, ylen=ylen, rrpi=rrpi, mnrad=mnrad, mxrad=mxrad, mnlon=mnlon, mxlon=mxlon

if n_params() eq 0 then begin
  print, 'Syntax:  Result = COMPRESS_REPROJ( rpi, xfac, [yfac], xlen=, ylen= )'
  print, 'With xfac and optional yfac, compresses using rebin.  Because new size must be an '
  print, 'integer multiple, may need to input (mnrad,mxrad,mnlon,mxlon) to adjust them.'
  retall
endif

sz = size(rpi)
if keyword_set(xlen) then begin
  if not keyword_set(ylen) then ylen=sz[2]
  out = congrid( rpi, xlen, ylen, /center, /interp, /minus_one )
  return, out
endif

if keyword_set(_xfac) then xfac=_xfac else xfac=1
if keyword_set(_yfac) then yfac=_yfac else yfac=1
out = rpi
if keyword_set(rrpi) then out1 = rrpi

xxs = sz[1] mod xfac
if keyword_set(xxs) then begin
  rpj_sz = size(out)
  @run_make_radi
  loni = loni[ xxs/2+(xxs mod 2) : sz[1]-xxs/2-1, * ]
  mnlon = min(loni) & mxlon = max(loni)
endif
out = out[ xxs/2+(xxs mod 2) : sz[1]-xxs/2-1, * ]
if keyword_set(rrpi) then out1 = out1[ xxs/2+(xxs mod 2) : sz[1]-xxs/2-1, * ]
yxs = sz[2] mod yfac
if keyword_set(yxs) then begin
  rpj_sz = size(out)
  @run_make_radi
  radi = radi[ yxs/2+(yxs mod 2) : sz[2]-yxs/2-1 ]
  mnrad = min(radi) & mxrad = max(radi)
endif
out = out[ *, yxs/2+(yxs mod 2) : sz[2]-yxs/2-1 ]
if keyword_set(rrpi) then out1 = out1[ *, yxs/2+(yxs mod 2) : sz[2]-yxs/2-1 ]
out = rebin( out, sz[1]/xfac, sz[2]/yfac )
if keyword_set(rrpi) then out1 = rebin( out1, sz[1]/xfac, sz[2]/yfac )

if keyword_set(rrpi) then rrpi = out1
return, out

end
