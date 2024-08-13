; Compare identified propeller-candidate locations from adjacent images, 
; look for any that appear in multiple images
sfile1 = 'image_coverage.sav'
if keyword_set(findfile(sfile1)) then restore, sfile1 else begin

  sfile2 = 'image_outlines.sav'
  if keyword_set(findfile(sfile2)) then restore, sfile2 else begin
    stop, 'First .run image_outltines'
  endelse
  if nf eq 1 then image_nums = reform(image_nums,1,1)
  outline_lon_corr = outline_lon - $
                     sqrt(caviar_omega2(outline_rad)) * 180/!dpi * $
                     ( rebin(image_nums,nf,nol) - image_num[nf/2] )

  dr = 10.0d0
  mnrad = round( min(outline_rad)/dr )*dr
  mxrad = round( max(outline_rad)/dr )*dr
  nrad = (mxrad-mnrad)/dr + 1
  gridrad = findgen(nrad)*dr+mnrad
  dl = 0.1d0
  mnlon = round( min(outline_lon_corr)/dl )*dl
  mxlon = round( max(outline_lon_corr)/dl )*dl
  nlon = (mxlon-mnlon)/dl + 1
  gridlon = findgen(nlon)*dl+mnlon
  grid = fltarr( nlon, nrad )
  for j=0,nf-1 do begin
    covered = polyfillv( interpol(findgen(nlon),gridlon,outline_lon_corr[j,*]),$
                         interpol(findgen(nrad),gridrad,outline_rad[j,*]), $
                         nlon, nrad )
    grid[covered] = 1
  endfor

  mnlon1 = 0.0d0
  mxlon1 = 360.0d0 - dl
  nlon1 = (mxlon1-mnlon1)/dl + 1
  gridlon1 = findgen(round(nlon1))*dl+mnlon1
  grid1 = fltarr( nlon1, nrad )
  j1 = round(( mnlon - fix_angles(mnlon,/to360) )/360)
  j2 = round(( mxlon - fix_angles(mxlon,/to360) )/360)
  for j=j1,j2 do begin
    foo = where( gridlon ge 360*j and gridlon lt 360*(j+1), count )
    startlon1 = abs( gridlon1 - fix_angles(min(gridlon[foo]),/to360) )
    startlon1 = (where( startlon1 eq min(startlon1) ))[0]
    foo1 = lindgen(count) + startlon1
    if (where( fix_angles( abs(gridlon[foo]-gridlon1[foo1]) ) gt 1e-10 ))[0] $
       ne -1 then stop
    grid1[foo1,*] = grid1[foo1,*] > grid[foo,*]
  endfor

  coverage = rebin( grid1, 1, nrad )

  save, mnrad, mxrad, mnlon, mxlon, mnlon1, mxlon1, nrad, nlon, nlon1, $
        gridrad, gridlon, gridlon1, grid, grid1, coverage, rev, obsname, $
        filename=sfile1

endelse

if keyword_set(dolzr) then begin
  lzr, 'image_coverage_'+rev+obsname, /half
  @plot_prepare
  plot_color
  !p.multi = [0,2,2]
endif

get_color
plot, tkm(gridrad), coverage, /nodata, /xs, /ys, yr=[0,1], $
      tit=rev+'/'+obsname, xtit='Radius'+tkmtit(), $
      ytit='Fractional longitudinal coverage'
polyfill, 136.77+[0,100,100,0,0], !y.crange[[0,0,1,1,0]], $
          noclip=0, co=ltgray()
polyfill, 136.505+[-.02,.02,.02,-.02,-.02], !y.crange[[0,0,1,1,0]], $
          noclip=0, co=ltgray()
polyfill, 133.585+[-.16,.16,.16,-.16,-.16], !y.crange[[0,0,1,1,0]], $
          noclip=0, co=ltgray()
axis, xaxis=0, /data, /xs, xtickn=replicate(' ',20)
axis, xaxis=1, /data, /xs, xtickn=replicate(' ',20)
axis, yaxis=0, /data, /xs, ytickn=replicate(' ',20)
axis, yaxis=1, /data, /xs, ytickn=replicate(' ',20)
oplot, tkm(gridrad), coverage

if keyword_set(dolzr) then clzr else stop

if not keyword_exists(plotgrid) then plotgrid = 1
if keyword_set(plotgrid) then begin
  plot, gridlon1, tkm(gridrad), /nodata, /xs, /ys, $
        xtit='Co-Rotating Longitude (!Uo!N)', ytit='Radius'+tkmtit()
  pts = wher( grid1 ne 0 )
  oplot, gridlon1[pts[0,*]], tkm(gridrad[pts[1,*]]), ps=3
endif

end
