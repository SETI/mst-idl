; Access SPICE kernels without using headers associated with a particular image.

if not keyword_set(cam_params) then begin

  ; Load general Cassini kernels
  cspice_furnsh,getenv("CAVIAR_KERNELS")
  ; Load Janus/Epimetheus kernels from Bob Jacobson
  cspice_furnsh, '/home/borogove/iss/NAIF/spk/051010BP_RE_90165_14363.bsp'
  cspice_furnsh, '/home/borogove/iss/NAIF/spk/sat215b.bsp'

  @get_cam_params

endif

if not ( keyword_set(date) and keyword_set(usedate) ) then begin
  print, 'Enter date in format YYYY-DOYTHH:MM:SS'
  date = ''
  read, date
endif

; Get ephemeris time (ET) from string date
cspice_str2et, date, et
next:
; Convert ET to encoded spacecraft clock (SCLKDP)
cspice_sce2c, sc, et, sclkdp
; Get C-kernel (camera pointing)
cspice_ckgp, inst, sclkdp, 1000.0d0, 'J2000', pmat, clkout, found
cmat = nacmat ## pmat

; Get spacecraft pointing in RA and Dec coordinates
pointing_ra_dec, cmat, RA, dec

@calculate_keywords

if keyword_set(iterate) then begin

  if not keyword_set(dt) then dt = 1  ; in seconds
  if not keyword_set(nt) then nt = 100
  if not keyword_set(ii) then ii = 0
  if ii eq 0 then begin
    if keyword_set(_keywords) then goto, printing else _keywords = keywords
  endif else _keywords = [ _keywords, keywords ]
  if ii lt nt-1 then begin
    ii = ii + 1
    et = et + dt
    goto, next
  endif

  printing:
  if not keyword_set(noprint) then begin
    nk = n_elements(_keywords)
    !p.multi = [0,2,2]
    if keyword_set(dolzr) then begin
      if not keyword_set(psname) then psname = 'plot'
      lzr, psname, /half
      @plot_prepare
      plot_color
    endif
    solid_small_circles
    if keyword_set(xxr) then begin
      int = lindgen( xxr[1]-xxr[0]+1 ) + xxr[0]
    endif else int = lindgen(nk)
    plot, findgen(nk)*dt/60, tkm(_keywords.ringplane_aimpoint_radius), ps=8, $
          /xs, /ys, xtit='Minutes after '+date, ytit='Radius'+tkmtit(), $
          yr = [min(tkm(_keywords[int].ringplane_aimpoint_radius)), $
                max(tkm(_keywords[int].ringplane_aimpoint_radius)) ], xma=[10,5]
    if keyword_set(t75) then oplot, ps=7, co=ctgreen(), [t75,t97,t119], $
              tkm([resloc(7,5,610),resloc(9,7,610),resloc(11,9,610)])
    plot, findgen(nk)*dt/60, _keywords.ringplane_aimpoint_longitude, ps=8, $
          /xs, /ys, xtit='Minutes after '+date, ytit='Longitude (!Uo!N)', $
          yr = [min(_keywords[int].ringplane_aimpoint_longitude), $
                max(_keywords[int].ringplane_aimpoint_longitude) ], xma=[5,10]
    if keyword_set(l75) then oplot, ps=7, co=ctgreen(), [t75,t97,t119], $
              [l75,l97,l119]
    plot, findgen(nk)*dt/60, _keywords.ringplane_aimpoint_radial_scale, ps=8, $
          /xs, /ys, xtit='Minutes after '+date, ytit='Radial Scale (km/px)', $
          yr = [min(_keywords[int].ringplane_aimpoint_radial_scale), $
                max(_keywords[int].ringplane_aimpoint_radial_scale) ], $
          xma=[10,5]
    if keyword_set(rs75) then oplot, ps=7, co=ctgreen(), [t75,t97,t119], $
              [rs75,rs97,rs119]
    plot, findgen(nk)*dt/60, _keywords.ringplane_aimpoint_phase_angle, ps=8, $
          /xs, /ys, xtit='Minutes after '+date, xma=[5,10], $
          ytit='Phase Angle (solid)', $
          yr = [min(_keywords[int].ringplane_aimpoint_phase_angle), $
                max(_keywords[int].ringplane_aimpoint_phase_angle) ]
    axis, yaxis=1, /ys, /save, ytit='Emission Angle (open)', $
          yr = [min(_keywords[int].ringplane_aimpoint_emission_angle), $
                max(_keywords[int].ringplane_aimpoint_emission_angle) ]
    open_small_circles
    oplot, findgen(nk)*dt/60, _keywords.ringplane_aimpoint_emission_angle, ps=8
    !p.multi = 0
    plot_blank, /noerase, tit=tit, xma=[10,10]
    if keyword_set(dolzr) then clzr
  endif

endif

end
