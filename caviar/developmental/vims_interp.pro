pro vims_interp, sfile, noplot=noplot, debug=debug

restore, sfile
nvims = n_elements(radii)
drvims = min(abs( radii[1:nvims-1] - radii[0:nvims-2] ))
dr = 0.02;10.0d0^( floor(alog10(drvims*2)) - 1 )
if drvims/dr lt 10 then stop
if drvims/dr gt 20 then stop
minrad = min(radii) > 74000
maxrad = max(radii) < 137000
minrad = long(minrad/dr)*dr
maxrad = long(maxrad/dr)*dr
_radi = dindgen( (maxrad-minrad)/dr + 1 )*dr + minrad
_val = interpol( data, radii, _radi )

if not keyword_set(noplot) then begin
  solid_small_circles
  plot, tkm(radii), data, /xs, /ys, xr=[118.82,118.87], ps=-4
  oplot, tkm(_radi), _val, ps=-8, color=ctred()
endif

filestem = strmid( sfile, 0, rstrpos(sfile,'.sav') )
foo = findfile(filestem+'_interp.sav')
if foo[0] ne '' then stop, filestem+'_interp.sav already exists.'
if keyword_set(debug) then stop
save, _radi, _val, filename=filestem+'_interp.sav'

end
