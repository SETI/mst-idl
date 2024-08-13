print, 'hello'
spawn, 'pwd', pwd
if strmid(pwd[0],rstrpos(pwd[0],'/',rstrpos(pwd[0],'/'))+1,1000) ne '046/RDHRESSCN' then stop, 'Must be in 046/RDHRESSCN'

restore, 'stretch.sav'
jjj = 14
image_name = filenames[jjj]
@caviar
nradi = n_elements(radi)
restore, 'fitparams.sav'
foo = where( _fitparams.jj eq jjj, count )
if count ne 2 then stop, 'Should be two density waves'

_val = val
vmodel = val*0
vmodel = rebin( vmodel, nradi, count )
for jfw=0,count-1 do begin
  rrname = _fitparams[jfw].rrname
  colon = strpos( rrname, ':' )
  space = [ rstrpos( rrname, ' ', colon ), $
            strpos( rrname, ' ', colon ) ]
  if space[1] eq -1 then space[1] = strlen(rrname)
  mm = float(strmid( rrname, colon+1, space[1]-colon-1 )) + 1
  rres = _fitparams[jfw].rres
  rres0 = rres
  _phase0 = _fitparams[jfw]._phase0
  phase0 = ( _phase0 + 45 ) mod 360
  sgma = _fitparams[jfw].sigma
  sgma_rres = abs( _fitparams[jfw].sigma_rres )
  sgma_phase = abs( _fitparams[jfw].sigma_phase )
  sgma_sgma = _fitparams[jfw].sigma_sigma
  fit1 = dblarr(3)
  fit1[2] = k_to_sigma(mm,rres) / (sgma*2*!pi/180)
  fit1[0] = fit1[2]*(rres-rres0)^2 + _phase0
  fit1[1] = -2*(rres-rres0)*fit1[2]
  zpt1 = -fit1[1] / fit1[2] / 2
  xx0 = min(where( radi gt rres ))
  radi1 = radi[xx0:nradi-1]
  pp = _fitparams[jfw].pp
  pp_sgma = _fitparams[jfw].pp_sigma
  _vmodel = fdensity_wave5( radi1, a=pp[0], xi_d=pp[1], mm=mm, phi=phase0, $
                 rres=rres, sigma=sgma, fresnel_integral=fresnel_integral )
  vmodel[xx0:nradi-1,jfw] = _vmodel
endfor

avgioverf = mean(_val[xx0:nradi-1])
input_val = 1
if count eq 2 then ii = 3 else begin
  stop, 'Indexing was specifically formulated for count=2.'
endelse
_val_im = fltarr(1024,1024,ii)
for i=0,ii-1 do begin
  print, i
  if i ge count then begin
    val = vmodel[*,i-2] + vmodel[*,i-1] + avgioverf
  endif else val = vmodel[*,i] + avgioverf
  @ns_radscan
  _val_im[*,*,i] = val_im
endfor
save, _val_im, filename='ring_atlas_mockup.sav'

end
