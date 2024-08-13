  if keyword_set(do13e5) then begin &$
    dr1 = 155 &$
    dr2 = 15 &$
  endif else if keyword_set(do1e5) then begin &$
    dr1 = 90 &$
    dr2 = 10 &$
  endif else begin &$
    dr1 = 150 &$
    dr2 = 20 &$
  endelse &$
  nr = 1500 &$
  radi = findgen(nr)/nr*dr1 + rres - dr2 &$
  val = fdensity_wave5( radi, a=a, xi_d=xi_d, mm=mm, phi=phi, $
                        rres=rres, sigma=sigma, xiout=xiout ) &$
  if keyword_set(noise) then begin &$
    _noise = randomn( seed, nr ) * noise &$
    val = val + _noise &$
    errbar = stddev(_noise) &$
  endif else errbar = .1 &$
  ;plot, radi-rres, val, xr=[0,125] &$
  save, radi, val, rres, sigma, phi, mm, xi_d, a, xiout, errbar, $
        filename=savefile
