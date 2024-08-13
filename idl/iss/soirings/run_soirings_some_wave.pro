; You should simply .run soirings_some_wave if you want to generate
; Figures some_wave1 and some_wave2.
; If you want to fit multiple iterations of synthetic waves, you should 
; keep running this program until it tells you you're done

if not keyword_exists(do13e5) then do13e5 = 1
savefile = '/home/borogove/matthewt/idl/iss/soirings/soirings_some_wave'
if keyword_set(do1e5) then savefile = savefile + '.1e5'
if keyword_set(do13e5) then savefile = savefile + '.13e5'
savefile = savefile + '.sav'

if not keyword_exists(donoise) then donoise = 1
seedfile = '/home/borogove/matthewt/idl/iss/soirings/soirings_some_wave_seed.sav'
if keyword_set(donoise) and keyword_set(findfile(seedfile)) then restore, seedfile else begin &$
  foo = randomu( seed ) &$
  save, seed, filename=seedfile &$
endelse
if not keyword_set(inparams) then begin &$

  mm = [ 9, 2, 30 ] &$
  if keyword_set(do13e5) then mm = 10 &$
  nmm = n_elements(mm) &$
  xi_d = [ 9, 20, 4 ] &$
  if keyword_set(do13e5) then xi_d = 10 &$
  if keyword_set(donoise) then if donoise eq 3 then xi_d = [ 7, 15 ] &$
  nxi = n_elements(xi_d) &$
  sigma = [ 40, 1, 10, 100 ] &$
  if keyword_set(do13e5) then sigma = 40 &$
  nsig = n_elements(sigma) &$
  phi = indgen(24)*15 &$ ;[ 0, 15, 90, 120, 180, 270 ] &$
  if keyword_set(donoise) then phi = indgen(12)*30 &$
  nphi = n_elements(phi) &$
  noise = [ 0 ] &$
  if keyword_set(donoise) then noise = [ 0, .1, .25, .5, 1, 2 ] &$
  if keyword_set(donoise) then if donoise eq 2 then $
                            noise = [ .05, .18, .38, .67, .83, 1.5 ] &$
  if keyword_set(donoise) then if donoise eq 3 then $
         noise = [ .05, .1, .18, .25, .38, .5, .67, .83, 1, 1.5, 2 ] &$
  nnoise = n_elements(noise) &$
  ninp = nmm * nxi * nsig * nphi * nnoise &$
  inparams = dblarr( ninp, 5 ) &$
  inparams[*,0] = mm[ findgen(ninp)/nxi/nsig/nphi/nnoise ] &$
  inparams[*,1] = xi_d[ findgen(ninp)/nsig/nphi/nnoise mod nxi ] &$
  inparams[*,2] = sigma[ findgen(ninp)/nphi/nnoise mod nsig ] &$
  inparams[*,3] = phi[ findgen(ninp)/nnoise mod nphi ] &$
  inparams[*,4] = noise[ findgen(ninp) mod nnoise ] &$

  outparams = [ [[inparams]], [[inparams]] ] * 0 &$
  jj = 0 &$

endif else jj = jj + 1

print, '***********'
print, strtrim(jj,2)+' / '+strtrim(ninp,2)
print, '***********'
if jj ge ninp then print, 'YOU''RE DONE!!!!!!!!!'
if jj ge ninp then retall
print, reform(inparams[jj,*])
print, '***********'

a = 1
xi_d = inparams[jj,1]
mm = inparams[jj,0]
phi = inparams[jj,3]
noise = inparams[jj,4]
rres = resloc(9,8,616)  ; Prometheus 9:8 at 128945.58 km
if keyword_set(do13e5) then rres = 1.3d5
sigma = inparams[jj,2]
if keyword_set(donoise) then if donoise eq 3 then fit_xi_d_only=1
@soirings_some_wave_get_savefile

;noplot = 1
.run soirings_some_wave
spawn, 'rm '+savefile

if keyword_set(donoise) then if donoise eq 3 then outparams[jj,1,0] = fit_xi_d
outparams[jj,*,0] = [ pp[0], fit_xi_d, fit_sigma, fit_phase0, fit_rres ]
outparams[jj,*,1] = [ errbar[0], sigma_xi_d, sigma_sigma, sigma_phase, $
                      sigma_rres ]

