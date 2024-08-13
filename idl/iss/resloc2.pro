function resloc_rsat_root, r
  common resloc1, lc82, omegasat, patternspeed, mm
  return, caviar_omega2(lc82=lc82,r) - omegasat^2
end

function resloc_rres_root_bw, r
  common resloc1, lc82, omegasat, patternspeed, mm
  return, sqrt(caviar_omega2(lc82=lc82,r)) - sqrt(caviar_nu2(lc82=lc82,r))/mm - patternspeed
end

function resloc_rres_root_dw, r
  common resloc1, lc82, omegasat, patternspeed, mm
  return, sqrt(caviar_omega2(lc82=lc82,r)) - sqrt(caviar_kappa2(lc82=lc82,r))/mm - patternspeed
end

function resloc_rres_root_corot, r
  common resloc1, lc82, omegasat, patternspeed, mm
  return, sqrt(caviar_omega2(lc82=lc82,r)) - patternspeed
end

; Calculate Resonance Locations
; Method and values based on Lissauer and Cuzzi, AJ 87, 1051 (1982)
function resloc2, rl1, rl2, omegasat=omegasat, kappasat=kappasat, nusat=nusat, $
	bending=bending, omegares=omegares, pp=_pp, kk=_kk, ll=ll, mm=mm, $
        gm=gm, cer=cer, cir=cir, patternspeed=patternspeed

if n_params() eq 0 then begin
  print, 'Syntax:  Result = RESLOC2( rl1, rl2, omegasat=, patternspeed= )'
  print, 'Returns the radial location (in km) of the rl1:rl2 Lindblad resonance.  Instead of identifying the satellite (as in resloc.pro), simply enter its orbital frequency omega (radians/sec), or the pattern speed (same units).'
  retall;
endif

common resloc1, _lc82, _omegasat, _patternspeed, _mm

; Use 5:4 resonance as default
if not keyword_exists(rl1) then rl1=5
if not keyword_exists(rl2) then rl2=4

; Lindblad and vertical resonances are generally written ll:(mm-1), 
; while corotation resonances are ll:mm.
ll = double(rl1)
if keyword_set(cer) or keyword_set(cir) then begin
  mm = double(rl2)
endif else mm = double(rl2) + 1
_mm = mm
if keyword_set(_pp) or keyword_set(_kk) then begin
  if keyword_set(_pp) then pp = _pp else pp = 0
  if keyword_set(_kk) then kk = _kk else kk = 0
endif else begin
  if keyword_set(bending) then begin
    pp = 1 * sign(ll)
    kk = ll - mm - pp
  endif else if keyword_set(cer) then begin
    pp = 0
    kk = ll - mm
  endif else if keyword_set(cir) then begin
    pp = ll - mm
    kk = 0
  endif else begin
    kk = ll - mm
    pp = 0
  endelse
endelse

gm = 37931289.4836 ;km^3/s^2
_gm = gm
; If pattern speed is specified, use it.  Otherwise, calculate from omegasat
if not keyword_set(patternspeed) then begin
  _omegasat = omegasat
  ; Start with provisional satellite orbital radius (fx_root wants a 3-element
  ; vector for reasons I don't understand)
  rsat0 = ( gm / omegasat^2 ) ^ (1./3) + [-5,0,5]
  ; Find precise satellite orbital radius 
  rsat = fx_root( rsat0, 'resloc_rsat_root', /double )
  ; Using radius, find satellite epicyclical frequency
  kappasat = sqrt( caviar_kappa2(lc82=lc82,rsat) )
  ; Vertical frequency can be simply calculated from the mean motion and the 
  ; epicyclic frequency (Shu et al 1983).
  nusat = sqrt( 2*omegasat^2 - kappasat^2 )
  ; Calculate pattern speed
  patternspeed = omegasat + kk/mm*kappasat + pp/mm*nusat
endif
_patternspeed = patternspeed

; Start with provisional resonance location
if keyword_set(omegasat) then freq=omegasat else freq=patternspeed
if abs(ll) eq 1 and mm eq 1 then begin
  ; Apsidal precession rate omega-kappa ~ sqrt(gm/r^3) * 3/2*j2*(prad/r)^2
  ; is set equal to satellite mean motion, then solve for r
  rres0 = ( gm / freq^2 * 9/4 * j2^2 * prad^4 ) ^ (1./7) + [-5,0,5]
  ; Nodal precession rate omega-mu is the negative of this, but same estimate.
endif else begin
  rres0 = ( gm / freq^2 / ll^2 * rl2^2 ) ^ (1./3) + [-5,0,5]
endelse
; Find precise resonance location
if keyword_set(bending) then begin
  rres = fx_root( rres0, 'resloc_rres_root_bw', /double )
endif else if keyword_set(cer) or keyword_set(cir) then begin
  rres = fx_root( rres0, 'resloc_rres_root_corot', /double )
endif else begin
  rres = fx_root( rres0, 'resloc_rres_root_dw', /double )
endelse
omegares = sqrt(caviar_omega2(lc82=lc82,rres))
if keyword_set(debug) then stop

return, rres

end
