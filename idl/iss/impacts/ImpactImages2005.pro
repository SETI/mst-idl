; usecurrentrings = 1
; noradscan = 1
; ring_rads = [ 82030.0d0, 82045, 84750, 84950, 85660, 85755, 85920, $
;               85990, 86375, 86605 ]
; ring_rads_legend = [ 'ER10a', 'ER10b', 'P5a', 'P5b', 'P6a', 'P6b', 'ER11a', $
;                      'ER11b', 'P7a', 'P7b' ]
; restore, 'stretch.sav'
; image_name = filenames[0]
; @caviar
; restore, 'prop_reproj_redge.sav'
; restore, 'et.sav'
; et = _et[0]
; imrad = _keywords[[0,2,4]].ringplane_aimpoint_radius
; imlon = _keywords[[0,2,4]].ringplane_aimpoint_longitude
; @draw_arrows
; save, ring_rads, ring_rads_legend, ring_coords, prop_reproj, et, cmat, $
;       imrad, imlon, arr_coords, filename='ImpactImages2005.sav'

if keyword_set(dolzr) then begin
  lzr, 'ImpactImages2005'
  @plot_prepare
  plot_color
  device, /cmyk
endif
if !d.name eq 'X' then device, decomposed=0

dir = getenv("DATA")+'/images/007/HIPHASE/'
image_name = 'N1493791286_1_cal.IMG'
rawim = read_vicar(dir+image_name)
run_histogram, rawim, stmin, stmax, locations=locations, many=many, hist=hist

restore, dir+'ImpactImages2005.sav'
nring = n_elements(ring_rads)
nper = 360
if n_elements(ring_coords[*,0]) ne nring*nper then stop

unget_color
notn = replicate(' ',20)
imdisp, /order, rawim>stmin<stmax, position=[.07,.1,.55,.98], /axis, $
        xtickn=notn, ytickn=notn, xtickle=1e-10, ytickle=1e-10
if !d.name eq 'X' then device, decomposed=0
ring_rads_legend[2:9] = ' '+ring_rads_legend[2:9]
ring_rads_legend[6:7] = ' '+ring_rads_legend[6:7]
for j=0,nring-1 do begin
  if j le 3 then begin
    xx = [-20,-100]
    yy = interpol( ring_coords[j*nper:(j+1)*nper-1,0], $
                   ring_coords[j*nper:(j+1)*nper-1,1], xx)
  endif else begin
    yy = [1044,1044+dy]
    xx = interpol( ring_coords[j*nper:(j+1)*nper-1,1], $
                   ring_coords[j*nper:(j+1)*nper-1,0], yy)
  endelse 
  oplot, xx, yy, noclip=1
  if j mod 2 eq 0 then begin
    xx1 = xx
    yy1 = yy
  endif else begin
    if !d.name eq 'X' then ddy = 6 else ddy = 10
    if j eq 1 then ddy = -6
    dx = max(xx) - min(xx)
    dy = max(yy) - min(yy)
    polyfill, [xx1,reverse(xx),xx1[0]], [yy1,reverse(yy),yy1[0]], co=ltgray()
    xyouts, mean([xx1[0],xx[1]]), mean([yy1[0],yy[1]])+ddy, $
            strmid( ring_rads_legend[j], 0, strlen(ring_rads_legend[j])-1 ), $
            orient=atan((yy[1]-yy[0])/(xx[0]-xx[1]))*180/!pi, align=.5
  endelse 
endfor

xyouts, 100, 300, 'Star Streak', co=blue()
arrow, 125, 305, 70, 375, hsize=!d.x_size/128, hthick=1, thick=3, /solid, $
       color=blue(), /data
name = [ 'To!CSaturn', 'Orbital!CMotion', 'Sun' ]
arr_coords[*,0] = 1024 - arr_coords[*,0]
arr_coords[*,0] = arr_coords[*,0] - 380
arr_coords[*,1] = arr_coords[*,1] + 100
align = [ .5, 0 ]
ddx = [ -20, 0 ]
for j=0,3,3 do begin
  arrow, arr_coords[j,1], arr_coords[j,0], $
         arr_coords[j+1,1], arr_coords[j+1,0], hsize=!d.x_size/128, $
         hthick=1, thick=3, /solid, /data
  xyouts, arr_coords[j+2,1]+ddx[j/3], arr_coords[j+2,0], align=align[j/3], $
          name[j/3]
endfor

@get_sat_prepare
@get_cam_params
dr = 200.0d0
dl = 0.3d0
imlon[2] = imlon[2]+0.08
pf = [ [81,86], [76,82], [80,102], [0,0], [78,85] ]
name = [ 'C1', 'C2', 'C3 & C4', '', 'C5' ]
clr = [ green(), green(), yellow(), yellow(), red() ] 
if !d.name eq 'X' then begin
  ddx = [ 13, 13, 13, 0, 0 ]
  ddy = [ 10, 10, 10, 0, -6 ]
  dy = 0.2
  name[1:2] = '  '+name[1:2]
  pos = [ [.6,.8,.77,.98], [.82,.8,.99,.98], [.6,.29,.77,.47], [0,0,0,0], $
          [.82,.29,.99,.47] ]
endif else begin
  ddx = [ 17, 17, 17, 0, 0 ]
  ddy = [ 20, 20, 20, 0, -6 ]
  dy = 0.15
  name[1:2] = '     '+name[1:2]
  pos = [ [.6,.745,.77,.865], [.82,.745,.99,.865], [.6,.36,.77,.48], $
          [0,0,0,0], [.82,.36,.99,.48] ]
endelse
cn = [ 3, 2, 2, 0, 1 ]
align = [ 1, 0, 0, 0, 0 ]
for j=0,4 do if j ne 3 then begin
  rad = prop_reproj[j].radlon[4] + [ dr, dr, -dr, -dr, dr ]
  lon = prop_reproj[j].radlon[5] + [ dl, -dl, -dl, dl, dl ]
  lon = lon + imlon[0] - imlon[prop_reproj[j].images]
  get_ring, et, rad, 0, 0, polera, poledec, 1, props, 699l, lons=lon
  image_coords, props, cmat, [[0],[0],[0]], cam_params, nl, coords
  oplot, coords[*,1], coords[*,0], co=clr[j]
  xyouts, coords[cn[j],1]+ddx[j], coords[cn[j],0]+ddy[j], co=clr[j], name[j], $
          orient=atan((coords[1,0]-coords[0,0])/$
                      (coords[0,1]-coords[1,1]))*180/!pi, align=align[j]
endif
for j=0,4 do if j ne 3 then begin
  rrpi = *(prop_reproj[j].rrpi)
  sz = size(rrpi)
  foo = wher( rrpi eq 0, count )
  mask = bytarr( sz[1], sz[2] ) + 1
  if count ne 0 then begin
    box = 1
    for k=0l,count-1 do begin
      mask[ (foo[0,k]-box)>0 : (foo[0,k]+box)<(sz[1]-1), $
            (foo[1,k]-box)>0 : (foo[1,k]+box)<(sz[2]-1) ] = 0
    endfor
    rrpi[where( mask eq 0 )] = median(rrpi[where( mask eq 1 )])
  endif 
  run_histogram, rrpi, stmin, stmax, threshold=0, /nocrop, /silent
  unget_color
  imdisp, rrpi>stmin<stmax, position=pos[*,j], /axis, $
          xtickn=notn, ytickn=notn, xtickle=1e-10, ytickle=1e-10
  if !d.name eq 'X' then device, decomposed=0
  clr = [ green(), green(), yellow(), yellow(), red() ] 
  oplot, !x.crange[[0,0,1,1,0]], !y.crange[[0,1,1,0,0]], co=clr[j], thick=4, $
         /noclip
  if !d.name eq 'X' then begin
    if j eq 2 then boxx = .25 else boxx = .1
    boxy = .12
  endif else begin
    if j eq 2 then boxx = .33 else boxx = .14
    boxy = .18
  endelse 
  polyfill, !x.crange[0]+(!x.crange[1]-!x.crange[0])*[0,0,boxx,boxx,0], $
            !y.crange[1]-(!y.crange[1]-!y.crange[0])*[0,boxy,boxy,0,0], $
            co=clr[j]
  xyouts, !x.crange[0]+(!x.crange[1]-!x.crange[0])*.02, co=0, $
            !y.crange[1]-(!y.crange[1]-!y.crange[0])*(boxy-.04), $
          strtrim(name[j],1)
  if j eq 0 then begin
    xyouts, 230, 40, 'Star!CStreak', co=blue()
    arrow, 225, 40, 160, 60, hthick=1, hsize=!d.x_size/128, thick=3, /solid, $
           color=blue(), /data
  endif
  if j eq 1 then begin
    arrow, !x.crange[0] - (!x.crange[1]-!x.crange[0])*.06, $
           !y.crange[1] + (!y.crange[1]-!y.crange[0])*.12, $
           !x.crange[0] - (!x.crange[1]-!x.crange[0])*.06, $
           !y.crange[1] - (!y.crange[1]-!y.crange[0])*.3, $
           hthick=1, hsize=!d.x_size/128, thick=3, /solid, /data
    arrow, !x.crange[0] - (!x.crange[1]-!x.crange[0])*.06, $
           !y.crange[1] + (!y.crange[1]-!y.crange[0])*.12, $
           !x.crange[0] + (!x.crange[1]-!x.crange[0])*.3, $
           !y.crange[1] + (!y.crange[1]-!y.crange[0])*.12, $
           hthick=1, hsize=!d.x_size/128, thick=3, /solid, /data
    xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.33, $
            !y.crange[1] + (!y.crange[1]-!y.crange[0])*.07, 'Orbital Motion'
    xyouts, !x.crange[0] - (!x.crange[1]-!x.crange[0])*.085, orient=270, $
            !y.crange[1] - (!y.crange[1]-!y.crange[0])*.33, 'To Saturn'
  endif 
  profhole = intarr(sz[2])
  profhole[pf[0,j]:pf[1,j]] = 1
  rrpi = fit_propellers4_subtractavg( rrpi, rrpi, mask, size(rrpi), $
                                      profhole=profhole )
  run_histogram, rrpi, stmin, stmax, threshold=0, /nocrop, /silent
  unget_color
  imdisp, rrpi>stmin<stmax, position=pos[*,j]-[0,dy,0,dy], /axis, $
          xtickn=notn, ytickn=notn, xtickle=1e-10, ytickle=1e-10
  if !d.name eq 'X' then device, decomposed=0
  clr = [ green(), green(), yellow(), yellow(), red() ] 
  oplot, !x.crange[[0,0,1,1,0]], !y.crange[[0,1,1,0,0]], co=clr[j], thick=4, $
         /noclip
endif

if keyword_set(dolzr) then clzr

end
