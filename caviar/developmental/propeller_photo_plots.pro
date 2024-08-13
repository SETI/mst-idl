restore, 'stretch.sav'
restore, 'fit_propellers4.sav'
restore, 'fit_propellers4_image_num.sav'

if keyword_set(dolzr) then begin
  psname = 'propeller_photo_plots'
  port = 1;0
  if keyword_set(average) then begin
    if average ge 2 then psname = psname + '_a2' else psname = psname + '_a1'
    if average ge 2 then port = 1
  endif 
  lzr, psname, port=port
  @plot_prepare
  plot_color
endif

if not keyword_exists(intbright) then intbright = 1
if not keyword_exists(plot_nperim) then plot_nperim = 1
if not keyword_exists(average) then average = 2;1

!p.multi = [0,3,2]
if keyword_set(intbright) then !p.multi[2] = !p.multi[2] + 1
if keyword_set(plot_nperim) then !p.multi[2] = !p.multi[2] + 1
!p.charsize = 2
!x.margin = 0
!x.omargin = [10,3]
!y.margin = 0
!y.omargin = [4,2]

phase = _keywords[image_num[where(_use)]].ringplane_aimpoint_phase_angle
emission = _keywords[image_num[where(_use)]].ringplane_aimpoint_emission_angle
sslon = _keywords[image_num[where(_use)]].ringplane_subsolar_longitude
sclon = _keywords[image_num[where(_use)]].ringplane_subspacecraft_longitude
aplon = _keywords[image_num[where(_use)]].ringplane_aimpoint_longitude
pp0 = _pp[0,where(_use)]
pp1 = _pp[1,where(_use)]
peakvol = 2*!pi*pp1* _pp[2,where(_use)] * _pp[3,where(_use)]

imlims = [ [0,10], [11,21], [22,32], [33,43], [44,51], [52,59], [67,74] ]

solid_small_circles
imindex = image_num[where(_use)]
imindex = uniq( imindex, sort(imindex) )
imnames = (image_num[where(_use)])[imindex]
nim = n_elements(imindex)
nperim = intarr(nim)
for j=0,nim-1 do begin
  foo = where( image_num[where(_use)] eq imnames[j], count )
  nperim[j] = count
endfor
if keyword_set(average) then begin
  _pp0 = pp0
  _pp1 = pp1
  _peakvol = peakvol
  pp0 = fltarr(nim)
  pp1 = fltarr(nim)
  peakvol = fltarr(nim)
  _phase = phase
  _emission = emission
  _sslon = sslon
  _sclon = sclon
  _aplon = aplon
  phase = fltarr(nim)
  emission = fltarr(nim)
  sslon = fltarr(nim)
  sclon = fltarr(nim)
  aplon = fltarr(nim)
  for j=0,nim-1 do begin
    foo = where( image_num[where(_use)] eq imnames[j], count )
    if count le 0 then stop
    pp0[j] = median(_pp0[foo])
    pp1[j] = median(_pp1[foo])
    peakvol[j] = median(_peakvol[foo])
    phase[j] = mean(_phase[foo])
    emission[j] = mean(_emission[foo])
    sslon[j] = mean(_sslon[foo])
    sclon[j] = mean(_sclon[foo])
    aplon[j] = mean(_aplon[foo])
  endfor 
  if average ge 2 then begin
    __imindex = imindex
    __imnames = imnames
    __nim = nim
    imindex = reform(imlims[0,*])
    imnames = __imnames[imindex]
    nim = n_elements(imindex)
    __pp0 = pp0
    __pp1 = pp1
    __peakvol = peakvol
    pp0 = fltarr(nim)
    pp0sd = fltarr(nim)
    pp1 = fltarr(nim)
    pp1sd = fltarr(nim)
    peakvol = fltarr(nim)
    peakvolsd = fltarr(nim)
    __phase = phase
    __emission = emission
    __sslon = sslon
    __sclon = sclon
    __aplon = aplon
    phase = fltarr(nim)
    emission = fltarr(nim)
    sslon = fltarr(nim)
    sclon = fltarr(nim)
    aplon = fltarr(nim)
    __nperim = nperim
    nperim = fltarr(nim)
    nperimsd = fltarr(nim)
    for j=0,nim-1 do begin
      if average ge 3 then begin
        foo = where( image_num[where(_use)] ge __imnames[imlims[0,j]] and $
                     image_num[where(_use)] lt __imnames[imlims[1,j]], count )
        if count le 0 then stop
        pp0[j] = median(_pp0[foo])
        pp0sd[j] = stddev(_pp0[foo]) / sqrt(count)
        pp1[j] = median(_pp1[foo])
        pp1sd[j] = stddev(_pp1[foo]) / sqrt(count)
        peakvol[j] = median(_peakvol[foo])
        peakvolsd[j] = stddev(_peakvol[foo]) / sqrt(count)
        phase[j] = median(_phase[foo])
        emission[j] = median(_emission[foo])
        sslon[j] = median(_sslon[foo])
        sclon[j] = median(_sclon[foo])
        aplon[j] = median(_aplon[foo])
      endif else begin
        foo = indgen( imlims[1,j]-imlims[0,j]+1 ) + imlims[0,j]
        if count le 0 then stop
        pp0[j] = mean(__pp0[foo])
        pp0sd[j] = stddev(__pp0[foo]) / sqrt(imlims[1,j]-imlims[0,j]+1)
        pp1[j] = mean(__pp1[foo])
        pp1sd[j] = stddev(__pp1[foo]) / sqrt(imlims[1,j]-imlims[0,j]+1)
        peakvol[j] = mean(__peakvol[foo])
        peakvolsd[j] = stddev(__peakvol[foo]) / sqrt(imlims[1,j]-imlims[0,j]+1)
        phase[j] = mean(__phase[foo])
        emission[j] = mean(__emission[foo])
        sslon[j] = mean(__sslon[foo])
        sclon[j] = mean(__sclon[foo])
        aplon[j] = mean(__aplon[foo])
      endelse
      nperim[j] = mean(__nperim[foo])
      nperimsd[j] = stddev(__nperim[foo]) / sqrt(imlims[1,j]-imlims[0,j]+1)
    endfor 
    yr1a = [20,140]
    yr1 = [.005,.0135]
    yr2 = [.00016,.00039]
    yr3 = [.0088,.0126]
    solid_circles
  endif else begin
    yr1a = [20,150]
    yr1 = [.0042,.0145]
    yr2 = [.00015,.00041]
    yr3 = [.0088,.0126]
  endelse
  ps = 8
endif else begin
  average = 0
  yr1a = [0,150]
  yr1 = [0,.04]
  yr2 = [0,.001]
  yr3 = [.0085,.0132]
  ps = 3
endelse
notn = replicate(' ',20)

if keyword_set(plot_nperim) then begin
  ytit = '# detected propellers per image'
  if keyword_set(average) then ind = findgen(nim) else ind = imindex
  plot, phase[ind], nperim, ps=8, yr=yr1a, /xs, /ys, $
        xtickn=notn, ytit=ytit, xr=[105,155]
  if average ge 2 then oploterr, phase[ind], nperim, nperimsd, 3
  plot, abs(90-emission[ind]), nperim, ps=8, yr=yr1a, /xs, /ys, $
        xtickn=notn, ytickn=notn, xr=[18,68]
  if average ge 2 then oploterr, abs(90-emission[ind]), nperim, nperimsd, 3
  if keyword_set(subsolar) then begin
    plot, abs(fix_angles(aplon[ind]-sslon[ind])), nperim, ps=8, yr=yr1a, $
          /xs, /ys, xtickn=notn, ytickn=notn, xr=[137,153]
    if average ge 2 then oploterr, abs(fix_angles(aplon[ind]-sslon[ind])), $
                                   nperim, nperimsd, 3
  endif else begin
    plot, abs(fix_angles(aplon[ind]-sclon[ind])), nperim, ps=8, yr=yr1a, $
          /xs, /ys, xtickn=notn, ytickn=notn, xr=[14,29]
    if average ge 2 then oploterr, abs(fix_angles(aplon[ind]-sclon[ind])), $
                                   nperim, nperimsd, 3
  endelse
endif

if keyword_set(intbright) then nj=2 else nj=1
for j=0,nj-1 do begin
  if j eq nj-1 then begin
    ytit = 'Propeller minus background, I/F'
    pp = pp1
    if average ge 2 then ppsd = pp1sd
    yr = yr2
  endif else begin
    ytit = 'Propeller integrated brightness, I/F*km!U2!N'
    pp = peakvol
    if average ge 2 then ppsd = peakvolsd
    yr = yr1
  endelse 
  plot_nosci, phase, pp, ps=ps, yr=yr, /xs, /ys, $
        xtickn=notn, ytit=ytit, xr=[105,155]
  if average ge 2 then oploterr, phase, pp, ppsd, 3
  plot_nosci, abs(90-emission), pp, ps=ps, yr=yr, /xs, /ys, $
        xtickn=notn, ytickn=notn, xr=[18,68]
  if average ge 2 then oploterr, abs(90-emission), pp, ppsd, 3
  if keyword_set(subsolar) then begin
    plot_nosci, abs(fix_angles(aplon-sslon)), pp, ps=ps, yr=yr, /xs, /ys, $
          xtickn=notn, ytickn=notn, xr=[137,153]
    if average ge 2 then oploterr, abs(fix_angles(aplon-sslon)), pp, ppsd, 3
  endif else begin
    plot_nosci, abs(fix_angles(aplon-sclon)), pp, ps=ps, yr=yr, /xs, /ys, $
          xtickn=notn, ytickn=notn, xr=[14,29]
    if average ge 2 then oploterr, abs(fix_angles(aplon-sclon)), pp, ppsd, 3
  endelse
endfor

ytit = 'Propeller (white) and background (red), I/F'
plot, phase, pp1+pp0, ps=ps, yr=yr3, /xs, /ys, $
      xtit='Phase Angle (!Uo!N)', ytit=ytit, xr=[105,155]
if average ge 2 then pp1m0sd = sqrt( pp1sd^2 + pp0sd^2 )
if average ge 2 then oploterr, phase, pp1+pp0, pp1m0sd, 3
oplot, phase, pp0, ps=ps, co=ctred()
if average ge 2 then for j=0,nim-1 do oplot, phase[[j,j]], $
                                             pp0[j]+pp0sd[j]*[-1,1], co=ctred()
plot, abs(90-emission), pp1+pp0, ps=ps, yr=yr3, /xs, /ys, $
      xtit='Ring Opening Angle (!Uo!N)', ytickn=notn, xr=[18,68]
if average ge 2 then oploterr, abs(90-emission), pp1+pp0, pp1m0sd, 3
oplot, abs(90-emission), pp0, ps=ps, co=ctred()
if average ge 2 then for j=0,nim-1 do oplot, abs(90-emission[[j,j]]), $
                                             pp0[j]+pp0sd[j]*[-1,1], co=ctred()
if keyword_set(subsolar) then begin
  plot, abs(fix_angles(aplon-sslon)), pp1+pp0, ps=ps, yr=yr3, /xs, /ys, $
        xtit='Local Solar Longitude (!Uo!N)', ytickn=notn, xr=[137,153]
  if average ge 2 then oploterr, abs(fix_angles(aplon-sslon)), $
                                 pp1+pp0, pp1m0sd, 3
  oplot, abs(fix_angles(aplon-sslon)), pp0, ps=ps, co=ctred()
  if average ge 2 then for j=0,nim-1 do oplot, $
     (abs(fix_angles(aplon-sslon)))[[j,j]], pp0[j]+pp0sd[j]*[-1,1], co=ctred()
endif else begin
  plot, abs(fix_angles(aplon-sclon)), pp1+pp0, ps=ps, yr=yr3, /xs, /ys, $
        xtit='Local Spacecraft Longitude (!Uo!N)', ytickn=notn, xr=[14,29]
  if average ge 2 then oploterr, abs(fix_angles(aplon-sclon)), $
                                 pp1+pp0, pp1m0sd, 3
  oplot, abs(fix_angles(aplon-sclon)), pp0, ps=ps, co=ctred()
  if average ge 2 then for j=0,nim-1 do oplot, $
     (abs(fix_angles(aplon-sclon)))[[j,j]], pp0[j]+pp0sd[j]*[-1,1], co=ctred()
endelse

if keyword_set(dolzr) then clzr

end
