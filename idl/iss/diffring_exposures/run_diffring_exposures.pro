pro run_diffring_exposures, allfilters=allfilters, filter=filter, bg=bg, $
  usenorm=usenorm, noplot=noplot, e_mnem=_e_mnem, g_mnem=_g_mnem, $
  e_phase=e_phase, g_phase=g_phase, fooe=fooe, foog=foog, ee=e, gg=g, $
  norme=norme, normg=normg, dn=dn, e_data=e_data, g_data=g_data, $
  fname=filtername, debug=debug, e_emission=e_emission, g_emission=g_emission

if keyword_set(findfile('diffring_exposures.sav')) then begin
  restore, 'diffring_exposures.sav'
endif else begin
  i0 = 1
  ; diffring_exposures.sav.old
  eruns = [ 2, 4, 5, 7, 8, 10, 13 ]
  gruns = [ 1, 3, 6, 9, 11, 12 ]
  ;; diffring_exposures.sav.new
  ;;eruns = [ eruns, 14, 16 ]
  ;;gruns = [ gruns, 15 ]
  eruns = [ eruns, 18 ]
  gruns = [ gruns, 17 ]
  i1 = max([ eruns, gruns ])

  for i=i0,i1 do begin

    filenames = '' & data = '' & dir = ''
    level = 0 & phase = 0 & emission = 0
    diffring_exposures, i, filenames, data, level, dir, phase, emission, $
                        inbox, verbose=0, old=old
    dir = replicate( dir, n_elements(filenames) )

    if i le i0+1 then begin
      if (where( i eq eruns ))[0] ne -1 then begin
        e_filenames = filenames
        e_data = data
        e_level = level
        e_dir = dir
        e_phase = phase
        e_emission = emission
        e_inbox = inbox
      endif else if (where( i eq gruns ))[0] ne -1 then begin
        g_filenames = filenames
        g_data = data
        g_level = level
        g_dir = dir
        g_phase = phase
        g_emission = emission
        g_inbox = inbox
      endif
    endif else begin
      if (where( i eq eruns ))[0] ne -1 then begin
        e_filenames = [ e_filenames, filenames ]
        e_data = [ [e_data], [data] ]
        e_level = [ e_level, level ]
        e_dir = [ e_dir, dir ]
        e_phase = [ e_phase, phase ]
        e_emission = [ e_emission, emission ]
        e_inbox = [ e_inbox, inbox ]
      endif else if (where( i eq gruns ))[0] ne -1 then begin
        g_filenames = [ g_filenames, filenames ]
        g_data = [ [g_data], [data] ]
        g_level = [ g_level, level ]
        g_dir = [ g_dir, dir ]
        g_phase = [ g_phase, phase ]
        g_emission = [ g_emission, emission ]
        g_inbox = [ g_inbox, inbox ]
      endif
    endelse

  endfor
  save, e_data, e_dir, e_emission, e_filenames, e_level, e_phase, e_inbox, $
        g_data, g_dir, g_emission, g_filenames, g_level, g_phase, g_inbox, $
        filename='diffring_exposures.sav'
endelse

if keyword_set(dn) then begin
  dn=4
  ytit = 'DN'
endif else begin
  dn=0
  ytit = 'I/F'
endelse
if keyword_set(bg) then begin
  e = e_level[*,6-dn]
  g = g_level[*,6-dn]
  ytit = 'Background '+ytit
endif else begin
  e = e_level[*,5-dn] - e_level[*,6-dn]
  g = g_level[*,5-dn] - g_level[*,6-dn]
  ytit = 'Core '+ytit
endelse
if keyword_set(allfilters) then begin
  filter = 0
  edone = 0
endif
nextfilter:
if not keyword_exists(filter) then filter = 1
filtername = [ 'All Filters', 'CL1/CL2', 'RED', 'BL1', 'GRN', 'VIO', 'UV1', $
               'IRP0', 'IRP90', 'IR1', 'IR2', 'IR3', 'P0', 'P60', 'P120', $
               'P0/P60/P120' ]
if keyword_set(bg) then begin
  ; Don't exclude saturated images
  egt0 = e eq e
  ggt0 = g eq g
endif else begin
  ; Do exclude saturated images
  egt0 = e gt 0
  ggt0 = g gt 0
endelse

if filter eq 0 then begin
  fooe = where( egt0, ce )
  foog = where( ggt0, cg )
endif else if filter eq 1 then begin
  fooe = where( e_data[4,*] eq 'CL1' and e_data[5,*] eq 'CL2' and egt0, ce )
  foog = where( g_data[4,*] eq 'CL1' and g_data[5,*] eq 'CL2' and ggt0, cg )
  ps = 4 & clr = ctwhite()
  if !d.name eq 'PS' then clr = 0
endif else if filter eq 2 then begin
  fooe = where( e_data[4,*] eq 'RED' or e_data[5,*] eq 'RED' and egt0, ce )
  foog = where( g_data[4,*] eq 'RED' or g_data[5,*] eq 'RED' and ggt0, cg )
  ps = 4 & clr = ctred()
endif else if filter eq 3 then begin
  fooe = where( e_data[4,*] eq 'BL1' or e_data[5,*] eq 'BL1' and egt0, ce )
  foog = where( g_data[4,*] eq 'BL1' or g_data[5,*] eq 'BL1' and ggt0, cg )
  ps = 4 & clr = ctblue()
endif else if filter eq 4 then begin
  fooe = where( e_data[5,*] eq 'GRN' and egt0, ce )
  foog = where( g_data[5,*] eq 'GRN' and ggt0, cg )
  ps = 4 & clr = ctgreen()
endif else if filter eq 5 then begin
  fooe = where( e_data[5,*] eq 'VIO' and egt0, ce )
  foog = where( g_data[5,*] eq 'VIO' and ggt0, cg )
  ps = 4 & clr = ctpurple()
endif else if filter eq 6 then begin
  fooe = where( e_data[4,*] eq 'UV1' and egt0, ce )
  foog = where( g_data[4,*] eq 'UV1' and ggt0, cg )
  ps = 4 & clr = ctcyan()
endif else if filter eq 7 then begin
  fooe = where( e_data[5,*] eq 'IRP0' and egt0, ce )
  foog = where( g_data[5,*] eq 'IRP0' and ggt0, cg )
  ps = 4 & clr = ctyellow()
endif else if filter eq 8 then begin
  fooe = where( e_data[5,*] eq 'IRP90' and egt0, ce )
  foog = where( g_data[5,*] eq 'IRP90' and ggt0, cg )
  ps = 4 & clr = ctorange()
endif else if filter eq 9 then begin
  fooe = where( e_data[5,*] eq 'IR1' and egt0, ce )
  foog = where( g_data[5,*] eq 'IR1' and ggt0, cg )
  ps = 6 & clr = ctred()
endif else if filter eq 10 then begin
  fooe = where( e_data[4,*] eq 'IR2' and egt0, ce )
  foog = where( g_data[4,*] eq 'IR2' and ggt0, cg )
  ps = 6 & clr = ctorange()
endif else if filter eq 11 then begin
  fooe = where( e_data[4,*] eq 'IR3' or e_data[5,*] eq 'IR3' and egt0, ce )
  foog = where( g_data[4,*] eq 'IR3' or g_data[5,*] eq 'IR3' and ggt0, cg )
  ps = 6 & clr = ctyellow()
endif else if filter eq 12 then begin
  fooe = where( e_data[4,*] eq 'P0' and egt0, ce )
  foog = where( g_data[4,*] eq 'P0' and ggt0, cg )
  ps = 6 & clr = ctgreen()
endif else if filter eq 13 then begin
  fooe = where( e_data[4,*] eq 'P60' and egt0, ce )
  foog = where( g_data[4,*] eq 'P60' and ggt0, cg )
  ps = 6 & clr = ctcyan()
endif else if filter eq 14 then begin
  fooe = where( e_data[4,*] eq 'P120' and egt0, ce )
  foog = where( g_data[4,*] eq 'P120' and ggt0, cg )
  ps = 6 & clr = ctblue()
endif else if filter eq 15 then begin
  fooe = where( ( e_data[4,*] eq 'P0' or e_data[4,*] eq 'P60' or $
                  e_data[4,*] eq 'P120' ) and egt0, ce )
  foog = where( ( g_data[4,*] eq 'P0' or g_data[4,*] eq 'P60' or $
                  g_data[4,*] eq 'P120' ) and ggt0, cg )
  ps = 6 & clr = ctgreen()
endif

if keyword_set(dn) then begin
  usenorm = 0
  foo = where( e_data[8,*] eq 'TABLE', count )
  if count gt 0 then e[foo] = e[foo] * 16
  foo = where( g_data[8,*] eq 'TABLE', count )
  if count gt 0 then g[foo] = g[foo] * 16
endif
if not keyword_exists(usenorm) then usenorm = 1
if keyword_set(usenorm) then begin
  if keyword_set(_e_mnem) then e_mnem=_e_mnem else e_mnem = '1'
  if ce gt 0 then norme = ( abs(cos(e_emission[fooe]*!pi/180)) > $
                            sin(!pi*e_mnem/180) )
  if keyword_set(_g_mnem) then g_mnem=_g_mnem else g_mnem = '0.12'
  if cg gt 0 then normg = ( abs(cos(g_emission[foog]*!pi/180)) > $
                            sin(!pi*g_mnem/180) )
  yr = [ 1.5e-7, 1.5e-5 ]
  ytit = ytit+' * cos(E)'
endif else begin
  norme = 1
  normg = 1
  if keyword_set(bg) then yr = [ 3e-6, 3e-2 ] else yr = [ 1e-5, 2e-3 ]
endelse
if keyword_set(dn) then yr=[1,4096]

filtercombos = [ [e_data[4,*]+e_data[5,*]], [g_data[4,*]+g_data[5,*]] ]
filtercombos = filtercombos[uniq( filtercombos, sort(filtercombos) )]

if keyword_set(noplot) then return

if not ( keyword_set(allfilters) and $
         ( keyword_set(edone) or filter ne 0 ) ) then begin
  !p.multi=[0,2,1]
  oldxm = !x.margin
  oldxom = !x.omargin
  !x.margin=0
  !x.omargin=[10,3]
  plot, /nodata, ps=4, /ylog, [35,165], yr, $
        /xs, /ys, xtit='Phase', ytit=ytit, tit='E Ring'
  if keyword_set(usenorm) then xyouts, 155, $
        10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*.05 ), $
        'Minimum (90-E) = '+e_mnem+'!Uo!N', align=1
endif
dych = (!y.crange[1]-!y.crange[0]) / (!d.y_size/!d.y_ch_size-6)
if keyword_set(allfilters) then begin
  if not keyword_set(edone) then begin
    if filter gt 0 then begin
      if ce gt 0 then oplot, e_phase[fooe], e[fooe]*norme, ps=ps, co=clr
      oplot, [40], [10^( !y.crange[1] - (filter*1.2+.2)*dych )], ps=ps, co=clr
      xyouts, 45, 10^( !y.crange[1] - (filter*1.2+.5)*dych ), $
              filtername[filter], co=clr, chars=1
    endif
    filter = filter + 1
    if filter ne 15 then goto, nextfilter else begin
      edone = 1
      filter = 0
    endelse
  endif
endif else begin
  if ce gt 0 then oplot, e_phase[fooe], e[fooe]*norme, ps=4
  xyouts, 40, 10^( !y.crange[1] - 1.7*dych ), filtername[filter], chars=1
endelse

if not ( keyword_set(allfilters) and $
         ( keyword_set(gdone) or filter ne 0 ) ) then begin
  plot, /nodata, ps=4, /ylog, [35,165], yr, $
          /xs, /ys, xtit='Phase', tit='G Ring', ytickn=replicate(' ',20)
  if keyword_set(usenorm) then xyouts, 155, $
        10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*.05 ), $
        'Minimum (90-E) = '+g_mnem+'!Uo!N', align=1
endif
if keyword_set(allfilters) then begin
  if filter eq 0 then begin
  endif else begin
    if cg gt 0 then oplot, g_phase[foog], g[foog]*normg, ps=ps, co=clr
  endelse
  filter = filter + 1
  if filter ne 15 then goto, nextfilter
endif else begin
  if cg gt 0 then oplot, g_phase[foog], g[foog]*normg, ps=4
endelse

!x.margin = oldxm
!x.omargin = oldxom
if keyword_set(debug) then stop

end

