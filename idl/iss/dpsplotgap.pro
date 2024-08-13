restore, '/home/borogove/iss/images/008/LPHRLFMOV/bigredge.sav'

if not keyword_exists(tstep) then tstep=1;0
rcen = [ 136505., 133586. ]
xxr = [ [-85000,-75000], [-1000,2500], [-67000,-47000], [-25000,-5000] ]
yyr = [ [15,23], [-23.5,-15.5], [148,156], [-175,-167] ]
if keyword_set(rev7) then begin
  xxr = [ [-85000,-75000], [-1000,2500], [-40000,-30000], [-97000,-82000] ]
  yyr = [ [15,23], [-23.5,-15.5], [145,153], [-175,-167] ]
endif
th = 5
!x.omargin = [ 0, float(!d.x_size)/!d.x_ch_size / 4 ]
tit='Keeler Gap';''
if tstep ne -1 then if keyword_set(dolzr) then begin
  lzr, '~/Desktop/KeelerEnckeSchem'+strtrim(tstep,2)
  @plot_prepare
  plot_color
endif else window, 0
if !d.name eq 'X' then device, decomposed=0
clr = [ red(), blue(), green(), purple() ]
clr = [ red(), green(), cyan(), purple() ]
solid_circles
!p.multi=[0,1,2]
!p.charsize=1
lontokm = !dpi/180*rcen[0]
xr=[-60,5];[-127.27,-40]
if tstep eq -1 then begin
  if keyword_set(dolzr) then begin
    lzr, '~/Desktop/KeelerEnckeSchem'+strtrim(tstep,2), /port
    @plot_prepare
    plot_color
  endif
  ; y-ranges for the two plots
  yr1 = [136470.1,136535]
  yr2 = [133402,133752]
  ; Ratio between the two y-intervals
  yrat = (yr2[1]-yr2[0]) / (yr1[1]-yr1[0])
  ; The height of the window, in characters
  winh = [ float(!d.y_size)/!d.y_ch_size ]
  ; The height of the Keeler Gap plot, in characters
  kph = winh/2 - 8
  ; Resize the Keeler Gap plot's height by this factor
  kphfac = 2. / (1+yrat)
  ; This would make the plot twice as high as the Keeler Gap plot
  yma = [ 4-kph, 4 ]
  ; Make the Encke plot yrat times as high as the Keeler plot 
  ; (ie, real proportions)
  ; Set kph*(1-kphfac)-kph*yrat*kphfac+kph = 0
  ; ==> kphfac = 2 / (1+yrat)
  yma = [ 4+kph*(1-kphfac)-kph*yrat*kphfac+kph, 4-kph*(1-kphfac) ]
  ; Other parameters
  ytit1 = 'Radial Distance from!C'+strtrim(long(rcen[0]),2)+' km'
  ytit2 = 'Radial Distance from '+strtrim(long(rcen[1]),2)+' km'
  rcen = rcen - [0,10]
  yti1 = 20
  yti2 = 40
  xtl1 = -.02/kphfac
  xtl2 = -.012
;  goto, encke
endif else begin
  yma = [4,4] 
  kph = 0
  kphfac = 1
  ytit1 = 'Radial Distance from '+strtrim(long(rcen[0]),2)+' km'
  ytit2 = 'Radial Distance from '+strtrim(long(rcen[1]),2)+' km'
  yti1 = 0
  yti2 = 0
  yr1 = [136475.1,136530]
  yr2 = [133392,133762]
  xtl1 = -.02
  xtl2 = -.02
endelse
plot, xr*lontokm, yr1-rcen[0], xs=9, ys=9, /nodata, tit=tit, $
     xminor=1, yminor=1, xtickle=xtl1, ytickle=-.02, xtickfo='(I7)', $
     yma=[4+kph*(1-kphfac),4], ytit=ytit1, yticki=yti1, $
     xtit='Azimuthal Distance (km) from Daphnis'
!x.crange = !x.crange / lontokm
if keyword_set(tit) then begin
  axis, xaxis=1, /save, xtickle=1e-10, xtickn=replicate(' ',20), /xs
endif else begin
  axis, xaxis=1, /save, xminor=1, xtickle=xtl1, $
     xticki=30, /xs, xtit='Degrees Longitude from Daphnis'
endelse
axis, yaxis=1, ytickle=1e-10, ytickn=replicate(' ',20)
polyfill, [reform(bigredge1[0,0:30000]),!x.crange[1],!x.crange[0]], $
    [reform(bigredge1[1,0:30000]),136470,136470]-rcen[0], noclip=0, co=gray()
polyfill, [reform(bigredge2[0,0:30000]),!x.crange[1],!x.crange[0]], $
    [reform(bigredge2[1,0:30000]),136535,136535]-rcen[0], noclip=0, co=gray()
;oplot, !x.crange, [1,1]*136505-rcen[0], l=1
oplot, noclip=1, !x.crange[[0,1,1,0,0]], !y.crange[[0,0,1,1,0]]

get_color
oplot, [0], [136505-rcen[0]], ps=8
if tstep ge 1 then begin
  ;stop
endif
if tstep ge 2 then begin
  oplot, xxr[[0,1,1,0,0],0]/lontokm, yyr[[0,0,1,1,0],0], co=clr[0], th=th
endif
if tstep ge 3 then begin
  oplot, xxr[[0,1,1,0,0],1]/lontokm, yyr[[0,0,1,1,0],1], co=clr[1], th=th
endif

encke:
restore, '/home/borogove/iss/images/mmhedman/8lphrlf_proc_may20.sav'
zz=out
zz[0,*]=zz[0,*]+360
if keyword_set(rev7) then begin
  restore, '/home/borogove/iss/images/mmhedman/LPHRLF_proc_may9.sav'
endif

if keyword_set(tit) then tit='Encke Gap'
lontokm = !dpi/180*rcen[1]
xr=[-60,5];[-127.27,-40]
baseline = smooth(zz[25,*],60) - 133415
if tstep ge 4 or tstep eq -1 then begin
  plot, xr*lontokm, yr2-rcen[1], xs=9, ys=9, /nodata, tit=tit, $
     xminor=1, yminor=1, xtickle=xtl2, ytickle=-.02, xtickfo='(I7)', $
     yma=yma, ytit=ytit2, $
     xtit='Azimuthal Distance (km) from Pan', yticki=yti2
  !x.crange = !x.crange / lontokm
  if keyword_set(tit) then begin
    axis, xaxis=1, /save, xtickle=1e-10, xtickn=replicate(' ',20), /xs
  endif else begin
    axis, xaxis=1, /save, xminor=1, xtickle=xtl2, $
       xticki=30, /xs, xtit='Degrees Longitude from Pan'
  endelse
  axis, yaxis=1, ytickle=1e-10, ytickn=replicate(' ',20)
  polyfill, [reform(zz[0,*]-360),!x.crange[1],!x.crange[0]], $
      [reform(zz[25,*]-baseline)-rcen[1],!y.crange[[0,0]]], noclip=0, co=gray()
  polyfill, [reform(zz[0,*]-360),!x.crange[1],!x.crange[0]], $
      [reform(zz[26,*]-baseline)-rcen[1],!y.crange[[1,1]]], noclip=0, co=gray()
  oplot, noclip=1, !x.crange[[0,1,1,0,0]], !y.crange[[0,0,1,1,0]]

  get_color
  oplot, [0], [0], ps=8
  if tstep eq -1 then begin
    ; Central radii (offset from rcen[1]) and widths of the three ringlets
    rwidth = [ 8., 8, 3, 9 ]
    rrcen = [ 0., -95.5, 138, 83.5 ]
    nn = 1000 * [ 1., 1, rwidth[2]/rwidth[0], 0.1 ]
    for k=0,3 do oplot, randomu(seed,nn[k])*(xr[1]-xr[0])+xr[0], $
                        randomn(seed,nn[k])*rwidth[k]+rrcen[k], ps=3
  endif else begin
    oplot, zz[0,*]-360, zz[2,*]-baseline-rcen[1], l=1, min=!y.crange[0], $
	max=!y.crange[1]
    oplot, zz[0,*]-360, zz[6,*]-baseline-rcen[1], l=1, min=!y.crange[0], $
	max=!y.crange[1]
    oplot, zz[0,*]-360, zz[10,*]-baseline-rcen[1], l=1, min=!y.crange[0], $
	max=!y.crange[1]
  endelse
endif
;if tstep eq -1 then goto, finish
if tstep ge 5 then begin
  oplot, xxr[[0,1,1,0,0],2]/lontokm, yyr[[0,0,1,1,0],2]+5*[-1,-1,1,1,-1], $
	co=clr[2], th=th
endif
if tstep ge 6 then begin
  oplot, xxr[[0,1,1,0,0],3]/lontokm, yyr[[0,0,1,1,0],3]+5*[-1,-1,1,1,-1], $
	co=clr[3], th=th
endif

!p.multi=[0,1,4]
!x.omargin = [ float(!d.x_size)/!d.x_ch_size * 3 / 2, 0 ]

if tstep ge 2 then begin
  plot, xxr[*,0]/lontokm, yyr[*,0], xs=1, ys=1, /nodata, $
       xtickle=1e-10, ytickle=1e-10, xma=[3,3], yma=[2,2], $
       xtickn=replicate(' ',20), ytickn=replicate(' ',20), /noerase
  polyfill, [reform(bigredge2[0,0:30000]),!x.crange[1],!x.crange[0]], $
         [reform(bigredge2[1,0:30000]),136530,136530]-rcen[0], noclip=0,$
	 co=gray()
  get_color
  oplot, noclip=1, !x.crange[[0,1,1,0,0]], !y.crange[[0,0,1,1,0]], co=clr[0], $
  	th=th*2
endif

if tstep ge 3 then begin
  !p.multi[0] = 3
  plot, xxr[*,1]/lontokm, yyr[*,1], xs=1, ys=1, /nodata, $
       xtickle=1e-10, ytickle=1e-10, xma=[3,3], yma=[2,2], $
       xtickn=replicate(' ',20), ytickn=replicate(' ',20), /noerase
  polyfill, [reform(bigredge1[0,0:30000]),!x.crange[1],!x.crange[0]], $
         [reform(bigredge1[1,0:30000]),136470,136470]-rcen[0], noclip=0, $
	 co=gray()
  get_color
  oplot, noclip=1, !x.crange[[0,1,1,0,0]], !y.crange[[0,0,1,1,0]], co=clr[1], $
	th=th*2
endif

if tstep ge 5 then begin
  !p.multi[0] = 2
  plot, xxr[*,2]/lontokm, yyr[*,2], xs=1, ys=1, /nodata, $
       xtickle=1e-10, ytickle=1e-10, xma=[3,3], yma=[2,2], $
       xtickn=replicate(' ',20), ytickn=replicate(' ',20), /noerase
  polyfill, [reform(zz[0,*]-360),!x.crange[1],!x.crange[0]], $
         [reform(zz[26,*]-baseline)-rcen[1],!y.crange[[1,1]]], noclip=0, $
	 co=gray()
  get_color
  oplot, noclip=1, !x.crange[[0,1,1,0,0]], !y.crange[[0,0,1,1,0]], co=clr[2], $
	th=th*2
endif

if tstep ge 6 then begin
  !p.multi[0] = 1
  plot, xxr[*,3]/lontokm, yyr[*,3], xs=1, ys=1, /nodata, $
       xtickle=1e-10, ytickle=1e-10, xma=[3,3], yma=[2,2], $
       xtickn=replicate(' ',20), ytickn=replicate(' ',20), /noerase
  polyfill, [reform(zz[0,*]-360),!x.crange[1],!x.crange[0]], $
         [reform(zz[25,*]-baseline)-rcen[1],!y.crange[[0,0]]], noclip=0, $
	 co=gray()
  get_color
  oplot, noclip=1, !x.crange[[0,1,1,0,0]], !y.crange[[0,0,1,1,0]], co=clr[3], $
	th=th*2
endif

finish:
!x.omargin = 0
if keyword_set(dolzr) then clzr

end
