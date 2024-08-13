  ;camera parameters for Bill Owen, JPL interoffice
  ;memorandum 312.E-2003-001
  na_cam_params=dblarr(12)
  na_cam_params[0]=2002.703     ;focal length NAC
  na_cam_params[1]=8.28d-6      ; epsilon2
  na_cam_params[2]=5.45d-6      ;epsilon5
  na_cam_params[3]=-19.67d-6    ;epsilon6
  na_cam_params[4]=83.33333333333333 ;Kx
  na_cam_params[5]=0.0               ;Kxy
  na_cam_params[6]=0.0               ;Kyx
  na_cam_params[7]=83.3428           ;Ky
  na_cam_params[8]=0.0               ;
  na_cam_params[9]=0.0               ;
  na_cam_params[10]=0.095            ;Big Omega
  na_cam_params[11]=3.0d-6           ;fov of one pixel
  wa_cam_params=dblarr(12)
  wa_cam_params[0]=200.7761     ;focal length WAC
  wa_cam_params[1]=60.89d-6     ; epsilon2
  wa_cam_params[2]=4.93d-6      ;epsilon5
  wa_cam_params[3]=-72.28d-6    ;epsilon6
  wa_cam_params[4]=83.33333333333333 ;Kx
  wa_cam_params[5]=0.0               ;Kxy
  wa_cam_params[6]=0.0               ;Kyx
  wa_cam_params[7]=83.34114          ;Ky
  wa_cam_params[8]=0.0               ;
  wa_cam_params[9]=0.0               ;
  wa_cam_params[10]=-0.018           ;Big Omega
  wa_cam_params[11]=3.0d-5           ;fov of one pixel
  if not keyword_set(cam_name) then cam_name = 'ISSNA'
  if cam_name eq 'ISSNA' then cam_params=na_cam_params 
  if cam_name eq 'ISSWA' then cam_params=wa_cam_params

  ; Set matrix of NAC pointing in Cassini reference frame
  nacmat = [[ -0.00148701973d0, -0.000171828726d0, 0.99999888d0 ],$
            [ -0.999998729d0, -0.000575703737d0, -0.00148711843d0 ],$
            [ 0.000575958621d0, -0.99999982d0, -0.000170972424d0 ]]

  ; Image size (256, 512, or 1024)
  if not keyword_set(nl) then nl = 1024

;  if keyword_set(oldpole) or not keyword_set(et) then begin &$
;    ;RA and dec of Saturn N pole from  pck00007.tpc, 
;    ;epoch 2004 JAN 01 12:00:00. TDB
;    polera=40.58756d0 &$ ;
;    poledec=83.53684d0 &$ 
;  endif else begin &$
;    scen = 365.25d0*86400*100 &$
;    omega1 = ( 24.058014d0 - 50.933966d0*et/scen )*!dpi/180 &$
;    omega2 = ( 325.758187d0 - 10.389768d0*et/scen )*!dpi/180 &$
;    omega3 = ( 234.873847d0 - 10.389768d0*et/scen )*!dpi/180 &$
;    polera = 40.596731d0 - 0.052461d0*et/scen - 0.031396d0*sin(omega1) - $
;             0.001791d0*sin(omega2) + 0.000101d0*sin(omega3) &$
;    poledec = 83.534290d0 - 0.005968d0*et/scen + 0.003517d0*cos(omega1) + $
;              0.000201d0*cos(omega2) + 0.000011*cos(omega3) &$
;;polera = polera - .1 &$
;;poledec = poledec + .1 &$
;  endelse

  ; Set the spacecraft index and instrument index equal to Cassini
  sc = -82L
  inst = -82000L
