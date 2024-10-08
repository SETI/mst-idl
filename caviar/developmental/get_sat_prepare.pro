; This loads polera, poledec, and Spice kernels for @get_sat_coords

kind = 'SPK'
cspice_ktotal, kind, spkcount
if spkcount eq 0 or keyword_set(reloadkernels) then $
  cspice_furnsh,getenv("CAVIAR_KERNELS")
if keyword_set(oldpole) or not keyword_set(et) then begin &$
  ;RA and dec of Saturn N pole from  pck00007.tpc, 
  ;epoch 2004 JAN 01 12:00:00. TDB
  polera=40.58756d0 &$ 
  poledec=83.53684d0 &$ 
endif else begin &$
  scen = 365.25d0*86400*100 &$
  omega1 = ( 24.058014d0 - 50.933966d0*et/scen )*!dpi/180 &$
  omega2 = ( 325.758187d0 - 10.389768d0*et/scen )*!dpi/180 &$
  omega3 = ( 234.873847d0 - 10.389768d0*et/scen )*!dpi/180 &$
  polera = 40.596731d0 - 0.052461d0*et/scen - 0.031396d0*sin(omega1) - $
           0.001791d0*sin(omega2) + 0.000101d0*sin(omega3) &$
  poledec = 83.534290d0 - 0.005968d0*et/scen + 0.003517d0*cos(omega1) + $
            0.000201d0*cos(omega2) + 0.000011*cos(omega3) &$
;polera = polera - .1 &$
;poledec = poledec + .1 &$
endelse

if not keyword_set(gspsilent) then print, 'Need to find your own ephemeris time (et), then run @get_sat_coords'

;end
