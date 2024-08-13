function restorque, rl1, rl2, satnum, sigma=sigma, x_nl=x_nl, x_max=x_max, $
        rres=rres, res_descrip=res_descrip, short=short, lc82=lc82, $
        dd_dr=dd_dr, patternspeed=patternspeed, omegares=omegares, $
        phi_slm=phi_slm, dphi_slm_dr=dphi_slm_dr, pp=pp, bending=bending

;, res_descrip=res_descrip, short=short, $
;	omegasat=omegasat, kappasat=kappasat, rsat=rsat, satnames=satnames, $
;	bending=bending, omegares=omegares, pp=pp, kk=kk, ll=ll, mm=mm, $
;        docalcs=docalcs, lc82=lc82, gm=gm, $
;        cer=cer, cir=cir, patternspeed=patternspeed

rres = resloc( rl1, rl2, satnum, res_descrip=res_descrip, short=short, $
	omegasat=omegasat, kappasat=kappasat, rsat=rsat, satnames=satnames, $
	bending=bending, omegares=_omegares, pp=pp, kk=kk, ll=ll, mm=mm, $
        lc82=lc82, may04=may04, gm=gm, esat=esat, isat=isat, $
        cer=cer, cir=cir, patternspeed=patternspeed )

;lc82 = 1
if keyword_set(lc82) then begin
  ; Convert km to cm, to conform with LC82.
  g_sigma100 = 6.672e-8*100       ; G * 100 g/cm^2, in cm / s^2
  rres = rres * 1e5               ; km ==> cm
  rsat = rsat * 1e5
  gm = gm * 1e15                  ; km^3/s^2 ==> cm^3/s^2
endif else begin
  g_sigma100 = 6.672e-8*100/1e5   ; G * 100 g/cm^2, in km / s^2
endelse

if keyword_set(lc82) then begin
  _gmsat = gm * [ .659e-7, .148e-6, 0, 0, 0, .237e-3, .259e-7, .331e-5, 0, $
                  .778e-8, .186e-8, 0, 0, 0, .177e-10, .124e-8, .766e-9, 0 ]
endif else begin
  ; From cpck02Aug2005.tpc
  _gmsat = [ 2.5268, 7.2089, 41.205, 73.113, 153.97, 8978.2, 0.37834, 120.49, $
             0.55323, 0.1261, .0350, .0017, .0048, .00024, .00072, .0128, $
             .0098, .00018 ]   ; km^3 / sec^2
endelse
gmsat = _gmsat[satnum-601]
beta = rres / rsat
if beta gt 1 then begin
  olr = 1
  beta = 1.0d0/beta
endif
lapl = laplace( beta, 0, mm, ll-mm+1 )  ; Creates array of Laplace coefficients
; For each resonance class, phi_slm is -GM/Rsat times the relevant
; argument from the Disturbing Function.  
if ll-mm eq 0 then begin
  ; First-order ILR.  From M&D 4D0.1 with j=m
  ; 0.5 * A_m (but the 0.5 goes away, see 8 Sept 2005 in my notebook)
  phi_slm = -gmsat / rsat * lapl[0,mm,0]
  dphi_slm_dr = -gmsat / rsat / rres * lapl[0,mm,1]
endif else if ll-mm eq 1 then begin
  ; Second-order ILR.  From M&D 4D1.2 with j=m+1
  ; e' * ( m + 0.5 + 0.5aD ) A_m
  phi_slm = -gmsat * esat / rsat * ( (mm+0.5)*lapl[0,mm,0] + 0.5*lapl[0,mm,1] )
  dphi_slm_dr = -gmsat * esat / rsat / rres * $
        ( (mm+1)*lapl[0,mm,1] + 0.5*lapl[0,mm,2] )
endif else if ll-mm eq 2 then begin
  ; Third-order ILR.  From M&D 4D2.3 with j=m+2
  ; e'^2 / 8 * [ ( 4m^2 + 9m + 4 ) + ( 4m + 6)aD + a^2D^2 ] A_m
  phi_slm = -gmsat * esat^2 / 8 / rsat * $
        ( (4*mm^2+9*mm+4)*lapl[0,mm,0] + (4*mm+5)*lapl[0,mm,1] + lapl[0,mm,2] )
  dphi_slm_dr = -gmsat * esat^2 / 8 / rsat / rres * $
        ( (4*mm^2+13*mm+9)*lapl[0,mm,1] + (4*mm+7)*lapl[0,mm,2] + lapl[0,mm,3] )
endif else if ll-mm eq 3 then begin
  ; Fourth-order ILR.  From M&D 3D3.4 with j=m+3
  ; e'^3 / 48 * [ ( 8m^3 + 42m^2 + 65m + 27 ) + ( 12m^2 + 51m + 51 )aD
  ;               + ( 6m + 15 )a^2D^2 + a^3D^3 ] A_m
  phi_slm = -gmsat * esat^3 / 48 / rsat * $
        ( (8*mm^3+42*mm^2+65*mm+27)*lapl[0,mm,0] + lapl[0,mm,3] + $
          (12*mm^2+51*mm+51)*lapl[0,mm,1] + (6*mm+15)*lapl[0,mm,2] )
  dphi_slm_dr = -gmsat * esat^3 / 48 / rsat / rres * $
        ( (8*mm^3+54*mm^2+116*mm+78)*lapl[0,mm,1] + lapl[0,mm,4] + $
          (12*mm^2+63*mm+81)*lapl[0,mm,2] + (6*mm+18)*lapl[0,mm,3] )
endif else if ll-mm eq 4 then begin
  ; Fifth-order ILR.  From M&D 4D4.5 with j=m+4
  ; e'^4 / 384 * [ ( 16m^4 + 152m^3 + 499m^2 + 646m + 256 ) 
  ;                + ( 32m^3 + 264m^2 + 692m + 568 )aD + a^4D^4 
  ;                + ( 24m^2 + 150m + 228 )a^2D^2 + ( 8m + 28 )a^3D^3 ] A_m
  phi_slm = -gmsat * esat^4 / 384 / rsat * $
        ( (16*mm^4+152*mm^3+499*mm^2+646*mm+256)*lapl[0,mm,0] + $
          (32*mm^3+264*mm^2+692*mm+568)*lapl[0,mm,1] + lapl[0,mm,4] + $
          (24*mm^2+150*mm+228)*lapl[0,mm,2] + (8*mm+28)*lapl[0,mm,3] )
  dphi_slm_dr = -gmsat * esat^4 / 384 / rsat / rres * $
        ( (16*mm^4+184*mm^3+763*mm^2+1338*mm+824)*lapl[0,mm,1] + $
          (32*mm^3+312*mm^2+992*mm+1024)*lapl[0,mm,2] + lapl[0,mm,5] + $
          (24*mm^2+174*mm+312)*lapl[0,mm,3] + (8*mm+32)*lapl[0,mm,4] )
endif else stop, ll, mm

if keyword_set(lc82) then begin
  ; Convert km to cm, to conform with LC82.
  omegares = sqrt(caviar_omega2( rres/1e5, domega_dr, /lc82 ))
  kappares = sqrt(caviar_kappa2( rres/1e5, dkappa_dr, /lc82 ))
  domega_dr = domega_dr / 1e5  ; radians/sec/km ==> radians/sec/cm
  dkappa_dr = dkappa_dr / 1e5
endif else begin
  omegares = sqrt(caviar_omega2( rres, domega_dr ))
  kappares = sqrt(caviar_kappa2( rres, dkappa_dr ))
endelse
; D = kappares^2 - mm^2*( omegares - patternspeed )^2
dd_dr = 2*kappares*dkappa_dr - 2*mm^2*( omegares - patternspeed )*domega_dr

; Units:  
; rres = km
; dd_dr = radians^2 / sec^2 / km
; phi_slm = km^2 / sec^2
; dphi_slm_dr = km / sec^2
; omegares, patternspeed = radians / sec
; ==> torqueoversigma = km^4 / sec^2
torqueoversigma = -mm * !dpi^2 / ( rres * dd_dr ) * $
       ( rres*dphi_slm_dr + 2*omegares*phi_slm/abs(omegares-patternspeed) )^2
;; Note that the following equation is what is actually in GT79.  But they
;; explicitly disavow concern with OLRs, and allowing omegares-patternspeed
;; to become negative (so that the dphi_slm_dr term and the phi_slm term are
;; not added but subtracted one from the other) causes the torque to become
;; about two orders of magnitude smaller for the OLR compared to ILR.  
;torqueoversigma = -mm * !dpi^2 / ( rres * dd_dr ) * $
;       ( rres*dphi_slm_dr + 2*omegares*phi_slm/(omegares-patternspeed) )^2

x_nl = 2*!dpi^2 / dd_dr * sqrt( -mm / rres / torqueoversigma ) * $
       g_sigma100^(3./2)
x_max = sqrt( -2 * torqueoversigma * 1.5e17 / !dpi / omegares / rres^4 )
;stop
return, torqueoversigma

end
