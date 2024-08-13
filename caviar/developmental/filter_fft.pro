function filter_fft, y, power, f, _fourier, pts=pts, xlog=xlog, ylog=ylog, base2=base2, xunits=xunits, xtit=xtit, ytit=ytit, tit=tit, ps=ps, xr=xr, yr=yr, coeff=coeff, nosignif=nosignif, noplot=noplot

if n_params() eq 0 then begin
  print, 'Syntax:  Result = FILTER_FFT( y, power, f, fourier, pts= )'
  retall
endif

if not keyword_set(y) then stop, 'y is not set.'
ny = n_elements(y)
if ny ne n_elements(f)*2 then stop, 'y does not agree with f.'

if not keyword_set(pts) then pts = -1
if pts[0] eq -1 then ptscount = 0l else ptscount = n_elements(pts)
!mouse.button = 0
if not keyword_set(noplot) then while !mouse.button ne 4 do begin

  if !mouse.button ne 0 then begin

    if yyy lt !y.crange[0] then begin
      retall
    endif
    dist = sqrt( (xx-xxx)^2 );+ (yy-yyy)^2 )
    cc = (where( dist eq min(dist) ))[0]
    if pts[0] eq -1 then pts = cc else begin
      foo = where( pts eq cc, count )
      if count eq n_elements(pts) then begin
        pts = -1
      endif else if count eq 0 then begin
        pts = [ pts, cc ]
      endif else pts = vec_remove( pts, foo )
    endelse
    pts = pts[ sort(pts) ]
    if pts[0] eq -1 then ptscount = 0l else ptscount = n_elements(pts)

  endif
  ps = -4
  plot_fft, y, power, f, xlog=xlog, ylog=ylog, base2=base2, xunits=xunits, $
            xtit=xtit, ytit=ytit, tit=tit, ps=ps, xr=xr, yr=yr, coeff=coeff, $
            nosignif=nosignif, xx=xx, yy=yy
  if ptscount gt 0 then begin
    oplot, [xx[pts]], [yy[pts]], ps=4, color=ctred()
    ; Remember ptscount is a long integer
    if ptscount gt 1 then for j=0,ptscount/2-1 do begin
      oplot, xx[pts[j*2]:pts[j*2+1]], yy[pts[j*2]:pts[j*2+1]], color=ctred()
    endfor
  endif
  print, 'Click on points to select, click again to de-select.'
  print, 'Click under the plot to quit.'
  print, 'Right-click to proceed with reverse FFT.  Regions in red will be excluded.'
  cursor, xxx, yyy, 3, /data

endwhile

if ptscount gt 1 then begin
  fourier = _fourier
  for j=0,ptscount/2-1 do begin
    fourier[ pts[j*2]+1 : pts[j*2+1]+1 ] = 0
    fourier[ ny-pts[j*2+1]-1 : ny-pts[j*2]-1 ] = 0
  endfor
  y1 = reconstruct_fft( fourier )
  return, y1
endif else return, y

end
