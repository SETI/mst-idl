if not keyword_set(noplot) then device, decompose=0
notn = replicate(' ',20)

if not keyword_exists(nowidth) then nowidth = 1
if keyword_set(paperplot) then begin
  case paperplot of
    1: dirs = [ '116/SATELLORB', '116/EQXSHADOW013', '116/EQXSHADOW001', '116/EQXSHADOW005', '169/HIPHASE' ]
    2: dirs = '007/HIPHASE' 
    3: dirs = '116/EQXSHADOW005' 
    else: stop
  endcase 
  if paperplot eq 1 then ppn = n_elements(dirs)
  basedir = '$DATA/images/'
  ppj = 0
  nopx = 1
  if keyword_set(dolzr) then begin
    lzr, '$HOME/idl/iss/impacts/calculate_ea_paperplot'+strtrim(paperplot,2)
    if paperplot eq 3 then clzr else begin
      @plot_prepare
      plot_color
    endelse
    !p.charsize = 1.3
    !x.margin = [8,8]
  endif else begin
    window, 8
    if paperplot ne 3 then window, 8, xs=!d.x_size*1.4, ys=!d.x_size
    if !d.x_size lt 1300 then !p.charsize = 1 else !p.charsize = 2
    !x.margin = [15,5]
  endelse 
  if paperplot eq 3 then !p.multi = [0,2,2] else !p.multi = [0,5,6];[0,5,4]
  nrows = 4   ; This is not necessarily the same as !p.multi[2]
  !y.margin = 0
  !y.omargin = [4,2]
  paperplot0:
  cd, basedir + dirs[ppj]
  if paperplot eq 1 then case ppj of
    0:  begin  ; 116/SATELLORB (A1)
      j1 = 1
      j2 = 1
      xy = 1
    end
    1:  begin  ; 116/EQXSHADOW013 (A2)
      j1 = 0
      j2 = 0
      xy = 1
    end
    2:  begin  ; 116/EQXSHADOW001 (B)
      j1 = 0
      j2 = 0
      xy = 1
    end
    3:  begin  ; 116/EQXSHADOW005 (C)
      j1 = 2
      j2 = 4
      xy = 1
    end
    4:  begin  ; 169/HIPHASE (C6)
      j1 = 1
      j2 = 1
      xy = 1
    end
  endcase 
  if paperplot eq 2 then begin
    j1 = 0
    j2 = 4
    xy = 1
  endif
  if paperplot eq 3 then begin
    j1 = 2
    j2 = 4
    xy = 1
  endif
endif else begin
  paperplot = 0
  ppj = 0
endelse
cdone = 0

if keyword_set(paperplot) or not keyword_set(prop_reproj) then begin
  rfile = 'prop_reproj_cea.sav'
  if keyword_set(findfile(rfile)) then restore, rfile else begin
    stop, 'Cannot find '+rfile
  endelse
  if paperplot eq 2 or paperplot eq 3 then ppn = n_elements(prop_reproj)
endif
npr = n_elements(prop_reproj)
if keyword_set(_j1) and not keyword_set(paperplot) then j1 = _j1
if keyword_set(_j2) and not keyword_set(paperplot)then j2 = _j2
if not keyword_exists(j1) then j1 = 0l
if not keyword_exists(j2) then j2 = npr - 1

spawn, 'pwd', pwd
pwd = pwd[0]
secondtolastslash = rstrpos( strmid(pwd,0,rstrpos(pwd,'/')), '/' )
pwd = strmid( pwd, secondtolastslash+1, 1000 )
sfile = 'calculate_ea.sav'
if keyword_set(findfile(sfile)) then restore, sfile else begin
  yyy = ptr_new(0)
  for j=1,npr-1 do yyy = [ yyy, ptr_new(0) ]
  nyyy = lonarr(npr)
  stmin = fltarr(npr)
  stmax = fltarr(npr)
  sm = intarr(npr,2)
  sf = intarr(npr)
  yind = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do yind = [ yind, [[ptr_new(0)],[ptr_new(0)]] ]
  cfit = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do cfit = [ cfit, [[ptr_new(0)],[ptr_new(0)]] ]
  lfit = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do lfit = [ lfit, [[ptr_new(0)],[ptr_new(0)]] ]
  qfit = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do qfit = [ qfit, [[ptr_new(0)],[ptr_new(0)]] ]
  csigma = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do csigma = [ csigma, [[ptr_new(0)],[ptr_new(0)]] ]
  lsigma = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do lsigma = [ lsigma, [[ptr_new(0)],[ptr_new(0)]] ]
  qsigma = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do qsigma = [ qsigma, [[ptr_new(0)],[ptr_new(0)]] ]
  llsigma = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do llsigma = [ llsigma, [[ptr_new(0)],[ptr_new(0)]] ]
  cstatus = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do cstatus = [ cstatus, [[ptr_new(0)],[ptr_new(0)]] ]
  lstatus = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do lstatus = [ lstatus, [[ptr_new(0)],[ptr_new(0)]] ]
  qstatus = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do qstatus = [ qstatus, [[ptr_new(0)],[ptr_new(0)]] ]
  ew = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do ew = [ ew, [[ptr_new(0)],[ptr_new(0)]] ]
  ew_nobgsub = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do ew_nobgsub = [ ew_nobgsub, [[ptr_new(0)],[ptr_new(0)]] ]
  ew_sigma = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do ew_sigma = [ ew_sigma, [[ptr_new(0)],[ptr_new(0)]] ]
  center = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do center = [ center, [[ptr_new(0)],[ptr_new(0)]] ]
  center_sigma = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do center_sigma = [ center_sigma, [[ptr_new(0)],[ptr_new(0)]] ]
  center_range = intarr(npr,2,2)
  slopefit = fltarr(npr,2,2)
  slopefit_sigma = fltarr(npr,2,2)
  theta = fltarr(npr,2)
  theta_sigma = fltarr(npr,2)
  width = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do width = [ width, [[ptr_new(0)],[ptr_new(0)]] ]
  width_sigma = [[ptr_new(0)],[ptr_new(0)]]
  for j=1,npr-1 do width_sigma = [ width_sigma, [[ptr_new(0)],[ptr_new(0)]] ]
  fituse = intarr(npr,2) + 4
  squeeze = fltarr(npr,2) + 0.1
  ea = fltarr(npr,2)
  ea_sigma = fltarr(npr,2)
  eatau = fltarr(npr,2)
  eatau_sigma = fltarr(npr,2)
  maxif = fltarr(npr,2)
  maxtau = fltarr(npr,2)
  cenbnd = lonarr(3,npr)
endelse

for j=j1,j2 do begin

  paperplot1c:
  rrpi = *( prop_reproj[j].rrpi )
  ; If the edge of the image seems to be included, then set contaminated
  ; values equal to image median to avoid confusing the fitting routine.
  sz = size(rrpi)
  foo = wher( rrpi eq 0, count )
  mask = bytarr( sz[1], sz[2] ) + 1
  if count gt 10 then begin
    print, 'rrpi contains zeroes, so assume it goes off the edge of the image.'
    box = 1;5
    for k=0l,count-1 do begin
      mask[ (foo[0,k]-box)>0 : (foo[0,k]+box)<(sz[1]-1), $
            (foo[1,k]-box)>0 : (foo[1,k]+box)<(sz[2]-1) ] = 0
    endfor
    rrpi[where( mask eq 0 )] = median(rrpi[where( mask eq 1 )])
  endif
  rrpi_orig = rrpi
  if not keyword_set(sf[j]) then sf[j] = 11
  rrpi_median = median( rrpi, sf[j] )
  foo = where( abs(rrpi-rrpi_median) gt 5*stddev(rrpi), count )
  if count gt 0 then rrpi[foo] = !values.f_nan
  ;rrpi = sigma_filter( rrpi_orig, sf[j] )
  if not keyword_set(rfac) then begin
    if pwd eq '116/EQXSHADOW001' or pwd eq '116/SATELLORB' then rfac = 3 else rfac = 5
  endif
  if paperplot eq 3 then rfac = 8
  rrpitv = rebin( rrpi, sz[1]*rfac, sz[2]*rfac, /sample )
  sztv = size(rrpitv)
  if not keyword_set(stmin[j]) then stmin[j] = min(rrpi)
  if not keyword_set(stmax[j]) then stmax[j] = max(rrpi)
  mnrad = prop_reproj[j].radlon[0]
  mxrad = prop_reproj[j].radlon[1]
  mnlon = prop_reproj[j].radlon[2]
  mxlon = prop_reproj[j].radlon[3]
  radx = make_radi( mnrad, mxrad, mnlon, mxlon, sz, loni=lonx )
  dlonx = mean( lonx[1:sz[1]-1] - lonx[0:sz[1]-2] )
  dradx = mean( radx[1:sz[2]-1] - radx[0:sz[2]-2] )
  _yyy = *yyy[j]

  if not keyword_set(noplot) then begin
    if paperplot eq 1 or paperplot eq 2 then begin
      unget_color
      imdisp, rrpitv>stmin[j]<stmax[j], xma=[10,10]
      !p.multi[0] = !p.multi[0] - !p.multi[1] + 1
    endif else begin
      if !d.name eq 'X' then window, 10, xs=sztv[1], ys=sztv[2]
      unget_color
      tvscl, rrpitv>stmin[j]<stmax[j]
      if paperplot eq 3 then begin
        if keyword_set(_yind) then for jj=2,4 do begin
          dcr = 0
          foo = where( _yind ge center_range[jj,xy,0] - dcr and $
                       _yind le center_range[jj,xy,1] + dcr, count ) 
          plots, _yind[foo]*rfac, $
                 poly( _yind[foo], slopefit[jj,xy,*] )*rfac, /dev, color=0
        endfor
        if keyword_set(dolzr) then begin
          outim = tvrd(true=tiffcolor)
          write_tiff, '$HOME/idl/iss/impacts/calculate_ea_paperplot'+$
                      strtrim(paperplot,2)+'.tiff', reverse( outim, 2 )
          lzr, '$HOME/idl/iss/impacts/calculate_ea_paperplot'+$
               strtrim(paperplot,2)
          @plot_prepare
          plot_color
        endif else wset, 8
      endif
    endelse
  endif
  !mouse.button = 4
  oldyyy = 0
  if keyword_set(paperplot) then goto, paperplot1
  if keyword_set(_yyy) then goto, plotline
  redo:
  yy = 0
  oldyyy = _yyy
  oldnyyy = nyyy
  drawnyet = 0

  print, 'Select as many points as needed to define a curve.'
  print, 'Right-click to proceed.'
  !mouse.button = 0
  while !mouse.button ne 4 do begin
    cursor, x, y, 3, /device
    x = x / rfac
    y = y / rfac
    if !mouse.button ne 4 then begin
      if not keyword_set(drawnyet) then begin
        yy = [[y],[x]]
        drawnyet = 1
      endif else yy = [ yy, [[y],[x]] ]
    endif
    if keyword_set(yy) then begin
      nyy = n_elements(yy[*,0])
      if nyy gt 2 then _yyy = [ yy, yy[0,*] ] else _yyy = yy
      *yyy[j] = _yyy
      nyyy[j] = n_elements(_yyy[*,0])
    endif
    plotline:
    wset, 10
    if not keyword_set(noplot) then begin
      unget_color
      tvscl, rrpitv>stmin[j]<stmax[j]
      solid_diamonds
      if keyword_set(oldyyy) then begin
        plots, oldyyy[0:oldnyyy[j]-1,1]*rfac + rfac/2, $
               oldyyy[0:oldnyyy[j]-1,0]*rfac + rfac/2, $
               ps=-8, color=ltcyan(), /device
      endif
      plots, _yyy[0:nyyy[j]-1,1]*rfac + rfac/2, $
             _yyy[0:nyyy[j]-1,0]*rfac + rfac/2, $
             ps=-8, color=green(), /device
    endif
  endwhile
  !mouse.button = 0

  reply = ''
  if keyword_set(paperplot) then reply = 'c'
  while reply eq '' do begin
    print, '[c]ontinue or [r]edo drawing or [q]uit?'
    read, reply
    case reply of
      'r':  goto, redo
      'c':  dummy = 1
      'q':  retall
      else:  reply = ''
    endcase 
  endwhile

  for xy=0,1 do begin

    paperplot1:
    if xy eq 0 then begin
      ; Calculate rad-by-rad (the original way, some variables stay the same)
      y0 = min(_yyy[*,0]) + 1
      y1 = max(_yyy[*,0]) - 1
      ny = y1 - y0 + 1
      _yind = indgen(ny) + y0
      if not keyword_set(noplot) then begin
        if not keyword_set(paperplot) then begin
          window, 8
          window, 8, xs=!d.x_size*1.4, ys=!d.x_size
          if !d.x_size lt 1000 then !p.charsize = 1 else !p.charsize = 2
          !p.multi = [0,4,5]
          nrows = 5   ; This is not necessarily the same as !p.multi[2]
          !y.margin = 0
          !y.omargin = [4,2]
          !x.margin = [15,5]
        endif
      endif
      _rrpi = rrpi
      szx = sz[1]
      szy = sz[2]
      xv = lonx
      yv = radx
      dx = dlonx * !pi/180 * mean(radx)
      dy = dradx
      if floor(alog10(dradx)) ge 0 then begin
        yfo = '(I'+strtrim(floor(alog10(max(radx)))+1,2)+')'
      endif else begin
        yfo = '(F'+strtrim(floor(alog10(max(radx)))+2-floor(alog10(dradx)),2)+$
              '.'+strtrim(-floor(alog10(dradx)),2)+')'
      endelse 
    endif else begin
      ; Calculate lon-by-lon
      y0 = min(_yyy[*,1]) + 1
      y1 = max(_yyy[*,1]) - 1
      ny = y1 - y0 + 1
      _yind = indgen(ny) + y0
      if not keyword_set(noplot) and $
         not keyword_set(paperplot) then !x.margin = [15,5]
      _rrpi = rotate( rrpi, 4 )
      szx = sz[2]
      szy = sz[1]
      xv = radx
      yv = lonx
      dx = dradx
      dy = dlonx * !pi/180 * mean(radx)
      if floor(alog10(dlonx)) ge 0 then begin
        yfo = '(I'+strtrim(floor(alog10(max(lonx)))+1,2)+')'
      endif else begin
        yfo = '(F'+strtrim(floor(alog10(max(lonx)))+2-floor(alog10(dlonx)),2)+$
              '.'+strtrim(-floor(alog10(dlonx)),2)+')'
      endelse 
    endelse 
    if sm[j,xy] ne 0 then begin
      _sm = sm[j,xy]
      __rrpi = rebin( _rrpi, szx, szy, 2*_sm+1 )
      for k=-_sm,_sm do __rrpi[*,*,k+_sm] = shift( _rrpi, 0, k )
      ;for k=-_sm,-1 do __rrpi[*,szy-1+k:szy-1,k+_sm] = !values.f_nan
      ;for k=1,_sm do __rrpi[*,0:k-1,k+_sm] = !values.f_nan
      for k=0,szy-1 do _rrpi[*,k] = mean( __rrpi[*,k,*], dim=3, /nan )
    endif 
    tracers = _yind[( findgen(5)/4 * (1-2*squeeze[j,xy]) + $
                     squeeze[j,xy] )*(ny-1)]
    ;tracers = _yind[findgen(5)/4*(ny-1)]
    ;tracers = round( findgen(5)/4 * (y1-y0) + y0 )
    _cfit = fltarr(1,ny)
    _lfit = fltarr(2,ny)
    _qfit = fltarr(3,ny)
    _csigma = fltarr(ny)
    _lsigma = fltarr(ny)
    _qsigma = fltarr(ny)
    _llsigma = fltarr(ny)
    _cstatus = intarr(ny)
    _lstatus = intarr(ny)
    _qstatus = intarr(ny)
    _ew = fltarr(ny)
    _ew_nobgsub = fltarr(ny)
    _ew_sigma = fltarr(ny)
    _center = fltarr(ny)
    _center_sigma = fltarr(ny)
    _width = fltarr(ny)
    _width_sigma = fltarr(ny)
    maxif[j,xy] = 0
    plotnum = 0
    for y=([y1,y0])[xy],([y0,y1])[xy],([-1,1])[xy] do begin
    ;for y=y1,y0,-1 do begin
    ;for y=y0,y1 do begin
      if xy eq 0 then begin
        xx = where( ( _yyy[*,0] ge y ) xor ( shift(_yyy[*,0],1) ge y ), count )
        if count ne 2 then stop
        x0 = [ interpol( _yyy[xx[0]-1:xx[0],1], _yyy[xx[0]-1:xx[0],0], y ), $
               interpol( _yyy[xx[1]-1:xx[1],1], _yyy[xx[1]-1:xx[1],0], y ) ]
        x1 = max(x0)
        x0 = min(x0)
      endif else begin
        xx = where( ( _yyy[*,1] ge y ) xor ( shift(_yyy[*,1],1) ge y ), count )
        if count ne 2 then stop
        x0 = [ interpol( _yyy[xx[0]-1:xx[0],0], _yyy[xx[0]-1:xx[0],1], y ), $
               interpol( _yyy[xx[1]-1:xx[1],0], _yyy[xx[1]-1:xx[1],1], y ) ]
        x1 = max(x0)
        x0 = min(x0)
      endelse 
      use1 = intarr(szx) + 1  ; Background pixels
      use2 = intarr(szx)      ; Feature pixels
      use3 = intarr(szx) + 1  ; All non-zero pixels
      use1[x0:x1] = 0
      use2[x0:x1] = 1
      nan = where( finite(_rrpi[*,y]) eq 0, nnan )
      if nnan gt 0 then begin
        use1[nan] = 0
        use2[nan] = 0
        use3[nan] = 0
      endif 
      zero = where( abs(_rrpi[*,y]) lt 1e-10, nzero )
      if nzero gt 0 then begin
        mkexed, zero, exed, z
        if z ne 0 then stop, 'More than one interval of zeroes?'
        use1[exed[0]:exed[1]] = 0
        use3[exed[0]:exed[1]] = 0
      endif 
      if not keyword_set(llsm) then llsm = 11
      if llsm mod 2 ne 1 then begin
        print, 'llsm should be odd.  Changing llsm from ' + strtrim(llsm,2) + $
               ' to ' + strtrim(llsm+1,2)+'.'
        llsm = llsm + 1
      endif 
      lluse1 = use1
      if x0-llsm-1 ge 0 then lluse1[0:x0-llsm-1] = 0
      lluse1[x0:szx-1] = 0
      lluse2 = use1
      lluse2[0:x1] = 0
      if x1+llsm+1 le szx-1 then lluse2[x1+llsm+1:szx-1] = 0
      foo = where( use1, nuse1 )
      foo = where( use2, nuse2 )
      foo = where( use3, nuse3 )
      fnz = where( use2 and use3, count )
      foo = where( lluse1, lln1 )
      foo = where( lluse2, lln2 )
      if count eq 0 then goto, fnz_skip  ; No non-zero pixels in feature
      yi = (where( y eq _yind, count ))[0]
      if count ne 1 then stop
      _cfit[*,yi] = svdfit( where(use1), _rrpi[where(use1),y], 1, $
                            sigma=sigma, status=status )
      _cstatus[yi] = status
      _lfit[*,yi] = svdfit( where(use1), _rrpi[where(use1),y], 2, $
                            sigma=sigma, status=status )
      _lstatus[yi] = status
      _qfit[*,yi] = svdfit( where(use1), _rrpi[where(use1),y], 3, $
                            sigma=sigma, status=status )
      _qstatus[yi] = status
      cbg = fltarr(szx)
      lbg = fltarr(szx)
      qbg = fltarr(szx)
      llbg = fltarr(szx)
      cbg[where(use3)] = poly( where(use3), _cfit[*,yi] )
      lbg[where(use3)] = poly( where(use3), _lfit[*,yi] )
      qbg[where(use3)] = poly( where(use3), _qfit[*,yi] )
      _csigma[yi] = stddev( _rrpi[where(use1),y] - cbg[where(use1)] )
      _lsigma[yi] = stddev( _rrpi[where(use1),y] - lbg[where(use1)] )
      _qsigma[yi] = stddev( _rrpi[where(use1),y] - qbg[where(use1)] )
      if lln1 eq 0 or lln2 eq 0 then begin
        if fituse[j,xy] eq 4 then begin
          print, (['Radius ','Longitude '])[xy] + strtrim(y,2) + ' has lln1=' +$
                 strtrim(lln1,2) + ' and lln2=' + strtrim(lln2,2) + $
                 '.  Cannot use locallinear fittype.'
          goto, query
        endif 
      endif else begin
        llsel = lindgen( max(where(lluse2)) - $
                         min(where(lluse1)) + 1 )+min(where(lluse1))
        llbg[min(where(lluse1)):max(where(lluse2))] = interpol( $
           [ mean(_rrpi[where(lluse1),y]), mean(_rrpi[where(lluse2),y]) ], $
           [ mean(where(lluse1)), mean(where(lluse2)) ], llsel )
        _llsigma[yi] = sqrt(stddev( _rrpi[where(lluse1),y] - $
                                    llbg[where(lluse1)] )^2 + $
                            stddev( _rrpi[where(lluse2),y] - $
                                    llbg[where(lluse2)] )^2 )
      endelse 

      ; Equivalent width is the integral of brightness d(distance)
      ; That is, sum of brightnesses in the interval multiplied by the
      ; number of pixels multiplied by the azimuthal size of a pixel in km. 
      case fituse[j,xy] of
        1: bg = cbg
        2: bg = lbg
        3: bg = qbg
        4: bg = llbg
      endcase 
      case fituse[j,xy] of
        1: sigma = _csigma[yi]
        2: sigma = _lsigma[yi]
        3: sigma = _qsigma[yi]
        4: sigma = _llsigma[yi]
      endcase 
      ; EW = Integral of ( I - BG ) dx
      ;    = Sum_i ( I_i - BG_i ) dx
      _ew[yi] = total( _rrpi[where(use2),y] - bg[where(use2)], /nan ) * dx
      maxif[j,xy] = maxif[j,xy] > max( _rrpi[where(use2),y] - bg[where(use2)] )
      ; What would EW be if we did not subtract the background? 
      _ew_nobgsub[yi] = total( _rrpi[where(use2),y], /nan ) * dx
      ; sigma_EW^2 = Sum_i ( d(EW)/d(I_i) )^2 sigma_I_i^2 + 
      ;              Sum_i ( d(EW)/d(BG_i) )^2 sigma_BG_i^2
      ;            = 2 Sum_i (dx)^2 sigma_I^2 = 2 N (dx)^2 sigma_I^2
      ; This is because the error bar on both I_i and BG_i is best estimated
      ; by the standard deviation of BG (sigma, which is a scalar here). 
      _ew_sigma[yi] = sqrt(2*nuse2) * dx * sigma
      ; Center = Integral of ( I - BG ) x dx / EW
      ;        = Sum_i ( I_i - BG_i ) x_i dx / EW
      _center[yi] = total(( _rrpi[where(use2),y] - $
                            bg[where(use2)] )*where(use2),/nan) * dx / _ew[yi]

      ; sigma_Center^2 = Sum_i ( d(Center)/d(I_i) )^2 sigma_I_i^2 + 
      ;                  Sum_i ( d(Center)/d(BG_i) )^2 sigma_BG_i^2
      ;                = 2 Sum_i ( x_i*dx/EW - 
      ;                            Center/EW d(EW)/d(I_i) ) sigma_I^2
      ;                = 2 Sum_i ( x_i - Center )^2 (dx/EW)^2 sigma_I^2
      _center_sigma[yi] = sqrt(2*total( (where(use2)-_center[yi])^2, /nan )) * $
                          dx / _ew[yi] * sigma
      ; Width^2 = Integral of ( I - BG ) ( x - Center )^2 dx / EW
      ;         = Sum_i ( I_i - BG_i ) ( x_i - Center )^2 dx / EW
      _width[yi] = sqrt( total(abs( _rrpi[where(use2),y] - bg[where(use2)] )*$
                               ( where(use2) - _center[yi] )^2,/nan) * $
                         dx / abs(_ew[yi]) )
      ; 2 Width d(Width)/d(I_i) = Sum_i [ (x_i-Center)^2 - Width^2 ] dx / EW
      ; sigma_Width^2 = 2 Sum_i ( d(Width)/d(I_i) )^2 sigma_I_i^2 + 
      ;                 2 Sum_i ( d(Width)/d(BG_i) )^2 sigma_I_i^2
      ;               = Sum_i [ (x_i-Center)^2 - Width^2 ]^2 (dx/EW/Width)^2
      ;                                                   sigma_I^2
      _width_sigma[yi] = sqrt(total(( (where(use2)-_center[yi])^2 - $
                                      _width[yi]^2 )^2,/nan)) * $
                         dx / _ew[yi] / _width[yi] * sigma

      fnz_skip:
      if (where( y eq tracers ))[0] ne -1 and $
         not keyword_set(noplot) and not keyword_set(paperplot) then begin
        solid_diamonds
        wset, 10
        if xy eq 0 then begin
          plots, [0,x0]*rfac + rfac/2, [y,y]*rfac + rfac/2, $
                 color=blue(), l=1, /device
          plots, [x1,szx]*rfac + rfac/2, [y,y]*rfac + rfac/2, $
                 color=blue(), l=1, /device
          if nzero gt 0 then begin
            plots, [exed[0],exed[1]]*rfac + rfac/2, [y,y]*rfac + rfac/2, $
                   color=cyan(), l=1, /device
          endif 
          plots, [x0,x1]*rfac + rfac/2, [y,y]*rfac + rfac/2, $
                 color=green(), l=1, /device
        endif else begin
          plots, [y,y]*rfac + rfac/2, [0,x0]*rfac + rfac/2, $
                 color=blue(), l=1, /device
          plots, [y,y]*rfac + rfac/2, [x1,szx]*rfac + rfac/2, $
                 color=blue(), l=1, /device
          if nzero gt 0 then begin
            plots, [y,y]*rfac + rfac/2, [exed[0],exed[1]]*rfac + rfac/2, $
                   color=cyan(), l=1, /device
          endif 
          plots, [y,y]*rfac + rfac/2, [x0,x1]*rfac + rfac/2, $
                 color=green(), l=1, /device
        endelse 
        wset, 8

        if plotnum eq nrows-1 then begin
          xtit = (['Longitude (px)','Radius (px)'])[xy]
          xtn = ''
        endif else begin
          xtit = ''
          xtn = notn
        endelse 
        plot, [0,szx], [min(_rrpi[where(_rrpi[*,y] ne 0),y]),max(_rrpi[*,y])], $
              /xs, /ynozero, /nodata, xtit=xtit, xtickn=xtn, ytit='I/F'
        oplot, _rrpi[*,y]
        xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.05, $
              !y.crange[1] - (!y.crange[1]-!y.crange[0])*.1, $
              (['Radius ','Longitude '])[xy] + strtrim(y,2) + ' (' + $
                string(yv[y],fo=yfo) + ([' km)','!Uo!N)'])[xy], chars=1
        if nzero gt 0 then begin
          oplot, zero, _rrpi[zero,y], color=cyan()
          oplot, [exed[0],exed[1]], _rrpi[[exed[0],exed[1]],y], $
                 color=cyan(), ps=8
        endif
        xsel = indgen(x1-x0+1) + x0
        oplot, xsel, _rrpi[xsel,y], color=green()
        oplot, [x0,x1], _rrpi[[x0,x1],y], color=green(), ps=8
        if xy eq 0 then begin
          if y ge center_range[j,xy,0] and y le center_range[j,xy,1] then $
             color=red() else color=purple()
        endif else begin
          if y ge center_range[j,xy,0] and y le center_range[j,xy,1] then $
             color=orange() else color=yellow()
        endelse 
        if keyword_set(*center[j,xy]) then begin
          oplot, [(*center[j,xy])[yi]], $
                 [_rrpi[ round((*center[j,xy])[yi])>0<(szx-1), y ]], $
                 color=color, ps=8
        endif 
        oplot, where(use3), cbg[where(use3)], color=red()
        oplot, where(use3), lbg[where(use3)], color=red(), l=2
        oplot, where(use3), qbg[where(use3)], color=red(), l=1
        if fituse[j,xy] eq 4 then begin
          oplot, where(lluse1), _rrpi[where(lluse1),y], color=cyan()
          oplot, where(lluse2), _rrpi[where(lluse2),y], color=cyan()
          oplot, llsel, llbg[llsel], color=cyan(), l=2
          oplot, [ mean(where(lluse1)), mean(where(lluse2)) ], $
                 [ mean(_rrpi[where(lluse1),y]), $
                   mean(_rrpi[where(lluse2),y]) ], color=cyan(), ps=8
        endif 
        if plotnum eq nrows-1 then begin
          !p.multi[0] = !p.multi[0] + !p.multi[1]*(nrows-1)
        endif else begin
          !p.multi[0] = !p.multi[0] - !p.multi[1] + 1
        endelse 
        plotnum = plotnum + 1

      endif 

    endfor 

    ; EA = Integral of EW dy = Sum_i EW_i dy
    ea[j,xy] = total(_ew,/nan) * dy
    ; sigma_EA^2 = Sum_i ( d(EA)/d(EW_i) )^2 sigma_EW_i^2
    ;            = Sum_i (dy)^2 sigma_EW_i^2
    ea_sigma[j,xy] = sqrt(total(_ew_sigma^2,/nan)) * dy
    pomega0 = 0.5
    if not keyword_set(qq) then qq=3
    restore, '$HOME/idl/iss/impacts/mie/mie_phase_functions.sav' 
    case pwd of
      '007/HIPHASE': begin ;C1 thru C5 (non-equinox)
        phaseang = ([174.67,173.97,172.76])[prop_reproj[j].images]
        ; From _keywords.ringplane_aimpoint_emission_angle
        emission = 71.25
        case j of
          0: print, pwd+'(C'+strtrim(j+1,2)+'): Use the Y fits for EA and to '+$
                    'show that the streak is horizontal'
          1: print, pwd+'(C'+strtrim(j+1,2)+'): Use the X fits for EA (fewer '+$
                    'background issues) and to show the cant angle'
          2: print, pwd+'(C'+strtrim(j+1,2)+'): Use the Y fits for EA and to '+$
                    'show that the streak is horizontal'
          3: print, pwd+'(C'+strtrim(j+1,2)+'): Use the Y fits for EA and to '+$
                    'show that the streak is horizontal'
          4: print, pwd+'(C'+strtrim(j+1,2)+'): Use the Y fits for EA and to '+$
                    'show that the streak is horizontal'
        endcase 
      end
      '116/SATELLORB': begin ;A1
        ; From _keywords.ringplane_aimpoint_phase_angle
        phaseang = 90.8
        ; From _keywords.ringplane_aimpoint_emission_angle
        emission = 70.0
        print, pwd+': The Y fits are more trustworthy than the X fits, ' +$
               'because the latter have background problems due to the cloud' +$
               ' going off the end of the image; this causes EA values to be '+$
               ' too low and slopes to trend away from the trouble spot.'
      end
      '116/EQXSHADOW001': begin ;B1
        ; From _keywords.ringplane_aimpoint_phase_angle
        phaseang = 150.6
        ; From _keywords.ringplane_aimpoint_emission_angle
        emission = 94.6
        print, pwd+': The X and Y fits seem equally trustworthy and should ' +$
               'be averaged together.'
      end
      '116/EQXSHADOW005': begin ;C
        ; From _keywords.ringplane_aimpoint_phase_angle
        phaseang = 149.3
        ; From _keywords.ringplane_aimpoint_emission_angle
        emission = 111.7
        print, pwd+': The Y fits are more trustworthy than the X fits, ' +$
               'because the background more reliably goes to zero at the ' +$
               'edges.  Also, it''s okay that the EA is much bigger than the '+$
               'impactclouds9 value because the latter included only the ' +$
               'central part of the feature.'
      end
      '116/EQXSHADOW013': begin ;A2
        ; From _keywords.ringplane_aimpoint_phase_angle
        phaseang = 100.3
        ; From _keywords.ringplane_aimpoint_emission_angle
        emission = 73.6
        print, pwd+': The Y fits are more trustworthy than the X fits, ' +$
               'because the feature extends to both edges in the horizontal ' +$
               'direction.  The first image has twice the S/N of the second.'
      end
      '169/HIPHASE': begin ;C6 (non-equinox)
        phaseang = ([0,0,174.78,174.65])[prop_reproj[j].images]
        ; From _keywords.ringplane_aimpoint_emission_angle
        emission = 102.2
        print, pwd+': The four fits (both X and Y from both images) are ' +$
               'equally trustworthy and should be averaged together.'
      end
      else: stop, 'Unrecognized observation'
    endcase 
    phasei = (where( abs(phaseang-phaseindex) eq min(abs(phaseang-phaseindex)) ))[0]
    phasefunc = phasefac[phasei,qq-min(qindex)]
    qmsg = string( qq, qindex, phasefac[phasei,qq-min(qindex)]/phasefac[phasei,*], $
                   fo='("   (EAtau uses phase function for q=",I1,"; for q=['+$
                   strjoin(replicate('",I1,",',n_elements(qindex)-1))+$
                   '",I1,"], multiply EAtau by ['+$
                   strjoin(replicate('",F6.3,",',n_elements(qindex)-1))+'",F6.3,"].")' )
    if keyword_set(mu1) then mu = 1 else mu = abs(cos(emission*!dpi/180))
    const = 4 * mu / pomega0 / phasefunc
    maxtau[j,xy] = const * maxif[j,xy]
    eatau[j,xy] = const * ea[j,xy]
    eatau_sigma[j,xy] = const * ea_sigma[j,xy]
    print, 'Maximum I/F ' + (['X','Y'])[xy] + ':  ' + strtrim(maxif[j,xy],2)
    print, 'Equivalent Area ' + (['X','Y'])[xy] + ' (I/F):  ' + $
           strtrim(ea[j,xy],2) + ' +- ' + strtrim(ea_sigma[j,xy],2) + ' km^2'
    print, 'Maximum tau ' + (['X','Y'])[xy] + ':  ' + strtrim(maxtau[j,xy],2)
    print, 'Equivalent Area ' + (['X','Y'])[xy] + ' (tau):  ' + $
           strtrim(eatau[j,xy],2) + ' +- ' + $
           strtrim(eatau_sigma[j,xy],2) + ' km^2'
    if keyword_set(qmsg) then print, qmsg
    foo = where( _yind ge center_range[j,xy,0] and $
                 _yind le center_range[j,xy,1], count )
    if count gt 0 then begin
      slopefit[j,xy,*] = svdfit( _yind[foo], _center[foo], 2, $
                                 measure_errors=_center_sigma[foo], $
                                 chisq=chisq, sigma=sigma )
      slopefit_sigma[j,xy,*] = sigma * sqrt(chisq/(count-2))
    endif else slopefit[j,xy,*] = 0
    ; In either case, express the angle as radius per unit longitude.
    ; slopefit is in px/px, so multiply by km/px on both top and bottom.
    if xy eq 0 then begin
      tan_theta = 1/slopefit[j,xy,1] * dy / dx
      tan_theta_sigma = 1/slopefit[j,xy,1]^2 * slopefit_sigma[j,xy,1] * dy / dx
    endif else begin
      tan_theta = slopefit[j,xy,1] * dx / dy
      tan_theta_sigma = slopefit_sigma[j,xy,1] * dx / dy
    endelse 
    theta[j,xy] = atan(-tan_theta)
    theta_sigma[j,xy] = 1 / (1+tan_theta^2) * tan_theta_sigma
    ; By Equation 4 of impactclouds10, 
    ; tan(theta) = 2 / ( 3 n t ) ==> t = 2 / ( 3 n tan(theta) )
    nn = sqrt(caviar_omega2( mean(radx) ))*3600 ; radians/hour
    age = 2./3 / nn / tan(theta[j,xy])
    age_sigma = 2./3 / nn / sin(theta[j,xy])^2 * theta_sigma[j,xy]
    print, 'Orientation ' + (['X','Y'])[xy] + ':  ' + $
           strtrim(theta[j,xy]*180/!dpi,2) + ' +- ' + $
           strtrim(theta_sigma[j,xy]*180/!dpi,2) + $
           ' deg (Kepler shear age: ' + strtrim(age,2) + ' +- ' + $
           strtrim(age_sigma,2) + ' hours)'
    *yind[j,xy] = _yind
    *cfit[j,xy] = _cfit
    *lfit[j,xy] = _lfit
    *qfit[j,xy] = _qfit
    *csigma[j,xy] = _csigma
    *lsigma[j,xy] = _lsigma
    *qsigma[j,xy] = _qsigma
    *llsigma[j,xy] = _llsigma
    *cstatus[j,xy] = _cstatus
    *lstatus[j,xy] = _lstatus
    *qstatus[j,xy] = _qstatus
    *ew[j,xy] = _ew
    *ew_nobgsub[j,xy] = _ew_nobgsub
    *ew_sigma[j,xy] = _ew_sigma    
    *center[j,xy] = _center   
    *center_sigma[j,xy] = _center_sigma    
    *width[j,xy] = _width  
    *width_sigma[j,xy] = _width_sigma    
    foo = where( abs(_center_sigma) gt 0.2*szx, count )
    if count gt 0 then begin
      _center[foo] = !values.f_nan
      _center_sigma[foo] = !values.f_nan
      _width[foo] = !values.f_nan
      _width_sigma[foo] = !values.f_nan
    endif 
    foo = where( _width lt 0 or _width gt 0.5*szx, count )
    if count gt 0 then begin
      _width[foo] = !values.f_nan
      _width_sigma[foo] = !values.f_nan
    endif 
    if keyword_set(paperplot) then begin
      case paperplot of
        1: xrpx = [0,szy]
        2: xrpx = [0,szy]
        3: begin
          foo = where( _yind ge center_range[j+1,xy,0] and $
                       _yind le center_range[j+2,xy,1], count )
          xrpx = [min(_yind[foo]),max(_yind[foo])]
          xrpx = xrpx + [-1,1]*(max(xrpx)-min(xrpx))*.1
        end 
      endcase 
    endif else begin
      xrpx = [min(_yind),max(_yind)]
    endelse 
    if keyword_set(nopx) then begin
      xr = interpol( yv, indgen(szy), xrpx )
      if xy eq 0 then xr = tkm(xr)
    endif 
    if keyword_set(paperplot) then goto, paperplot2
    if not keyword_set(noplot) then begin

      wset, 10
      foo = where( _yind ge center_range[j,xy,0] and $
                   _yind le center_range[j,xy,1], count )
      if rfac ge 3 then ps=8 else ps=3
      solid_small_circles
      if count gt 0 then case xy of
        0:  plots, _center[foo]*rfac + rfac/2, _yind[foo]*rfac + rfac/2, $
                   ps=ps, color=red(), /device
        1:  plots, _yind[foo]*rfac + rfac/2, _center[foo]*rfac + rfac/2, $
                   ps=ps, color=orange(), /device
      endcase 
      foo = where( _yind lt center_range[j,xy,0] or $
                   _yind gt center_range[j,xy,1], count )
      if count gt 0 then case xy of
        0:  plots, _center[foo]*rfac + rfac/2, _yind[foo]*rfac + rfac/2, $
                   ps=ps, color=purple(), /device
        1:  plots, _yind[foo]*rfac + rfac/2, _center[foo]*rfac + rfac/2, $
                   ps=ps, color=yellow(), /device
      endcase 
      wset, 8

      !x.margin = [10,10]
      plot, xrpx, [min([_cfit,_lfit[0,*],_qfit[0,*]]),$
                   max([_cfit,_lfit[0,*],_qfit[0,*]])], $
            xs=5, /ys, /nodata, ytit='Constant Term'
      if !d.name eq 'X' then clr = white() else clr = 0
      if fituse[j,xy] eq 1 then clr = red()
      oplot, _yind, _cfit, color=clr
      foo = where( _cstatus, count )
      if count gt 0 then oplot, _yind[foo], _cfit[foo], ps=7, color=clr
      if !d.name eq 'X' then clr = white() else clr = 0
      if fituse[j,xy] eq 2 then clr = red()
      oplot, _yind, _lfit[0,*], l=2, color=clr
      foo = where( _lstatus, count )
      if count gt 0 then oplot, _yind[foo], _lfit[0,foo], ps=7, color=clr
      if !d.name eq 'X' then clr = white() else clr = 0
      if fituse[j,xy] eq 3 then clr = red()
      oplot, _yind, _qfit[0,*], l=1, color=clr
      foo = where( _qstatus, count )
      if count gt 0 then oplot, _yind[foo], _qfit[0,foo], ps=7, color=clr
      if keyword_set(nopx) then begin
        axis, xaxis=0, /xs, xr=xr, /save, xtickn=notn
        axis, xaxis=1, /xs, xr=!x.crange, xtickn=notn
      endif else begin
        axis, xaxis=0, /xs, xr=!x.crange, xtickn=notn
              ;xtit=(['Radius (px)','Longitude (px)'])[xy]
        axis, xaxis=1, /xs, xr=!x.crange, xtickn=notn
      endelse
      !p.multi[0] = !p.multi[0] - !p.multi[1] + 1

      plot, xrpx, [min([_lfit[1,*],_qfit[1,*]]),$
                   max([_lfit[1,*],_qfit[1,*]])], $
            xs=5, ys=9, /nodata, ytit='Linear Term'
      if !d.name eq 'X' then clr = white() else clr = 0
      if fituse[j,xy] eq 2 then clr = red()
      oplot, _yind, _lfit[1,*], l=2, color=clr
      if !d.name eq 'X' then clr = white() else clr = 0
      if fituse[j,xy] eq 3 then clr = red()
      oplot, _yind, _qfit[1,*], l=1, color=clr
      axis, yaxis=1, /ys, yr=[min(_qfit[2,*]),max(_qfit[2,*])], /save, $
            ytit='Quadratic Term (dot-dash)'
      oplot, _yind, _qfit[2,*], l=3, color=clr
      if keyword_set(nopx) then begin
        axis, xaxis=0, /xs, xr=xr, /save, xtickn=notn
        axis, xaxis=1, /xs, xr=!x.crange, xtickn=notn
      endif else begin
        axis, xaxis=0, /xs, xr=!x.crange, xtickn=notn
              ;xtit=(['Radius (px)','Longitude (px)'])[xy]
        axis, xaxis=1, /xs, xr=!x.crange, xtickn=notn
      endelse
      !p.multi[0] = !p.multi[0] - !p.multi[1] + 1

      paperplot2:
      if keyword_set(paperplot) then begin
;      if paperplot eq 1 then begin
        ;if ppj eq 3 then begin
        ;  foo = where( _yind ge center_range[j+1,xy,0] and $
        ;               _yind le center_range[j+2,xy,1], count )
        ;endif else begin
        ;  foo = where( _yind ge center_range[j,xy,0] and $
        ;               _yind le center_range[j,xy,1], count )
        ;endelse 
        ;yr = [min(_center[foo]-_center_sigma[foo],/nan)>0,$
        ;      max(_center[foo]+_center_sigma[foo],/nan)<szx]
        ;dyr = yr[1] - yr[0]
        ;yr = yr + [-.1,.1]*dyr
        if paperplot eq 3 then begin
          foo = where( _yind ge center_range[j+1,xy,0] and $
                       _yind le center_range[j+2,xy,1], count )
          yr = [min(_center[foo]-_center_sigma[foo],/nan)>0,$
                max(_center[foo]+_center_sigma[foo],/nan)<szx]
          yr = yr + [-1,1]*(max(yr)-min(yr))*.1
        endif else yr = [0,szx]
        q1 = 1 & q2 = 1
      endif else begin
        yr = [min(_center-_center_sigma,/nan)>0,$
              max(_center+_center_sigma,/nan)<szx]
        q1 = 0 & q2 = 2
        dyr = yr[1] - yr[0]
        yr = yr + [-.1,.1]*dyr
      endelse 
      if paperplot eq 1 then begin
        tit=(['Ax(1)','Ax(2)','Bx','Cx','C6'])[ppj]
      endif else if paperplot eq 2 then begin
        tit=(['C1','C2','C3','C4','C5'])[j]
      endif else tit=''
      plot, xrpx, $
            ;[min(_center,/nan),max(_center,/nan)], $
            yr, xs=5, ys=5, /nodata, tit=tit
      paperplot3a:
      if not keyword_set(center_range[j,xy,1]) then begin
        center_range[j,xy,1] = szy
      endif 
      for q=q1,q2 do begin
        case q of
          0: foo = where( _yind lt center_range[j,xy,0], count )
          1: foo = where( _yind ge center_range[j,xy,0] and $
                          _yind le center_range[j,xy,1], count )
          2: foo = where( _yind gt center_range[j,xy,1], count )
        endcase 
        if count gt 0 then begin
          if q eq 1 then case xy of
            0:  color = red()
            1:  color = orange()
          endcase 
          if q ne 1 then case xy of
            0:  color = purple()
            1:  color = yellow()
          endcase 
          if keyword_set(paperplot) then color = gray()
          if paperplot eq 2 and ( j eq 1 or j eq 4 ) then begin
            foo = where( *yind[j,1-xy] ge center_range[j,1-xy,0] and $
                         *yind[j,1-xy] le center_range[j,1-xy,1], count )
            polyfill, [(*center[j,1-xy])[foo]-(*center_sigma[j,1-xy])[foo],$
                       reverse( (*center[j,1-xy])[foo]+$
                                (*center_sigma[j,1-xy])[foo])], $
                      [(*yind[j,1-xy])[foo],reverse((*yind[j,1-xy])[foo])], $
                      color=color, noclip=1
            oplot, (*center[j,1-xy])[foo], (*yind[j,1-xy])[foo]
          endif else begin
            polyfill, [_yind[foo],reverse(_yind[foo])], $
                      [_center[foo]-_center_sigma[foo],$
                       reverse(_center[foo]+_center_sigma[foo])], $
                      color=color, noclip=0
            oplot, _yind[foo], _center[foo]
          endelse 
        endif 
      endfor
      if ( paperplot eq 1 and ppj eq 3 ) or paperplot eq 3 then begin
        if j lt j2 then begin
          j = j + 1
          goto, paperplot3a
        endif else j = j1
      endif 
      if keyword_set(*center[j,1-xy]) and not keyword_set(paperplot) then begin
        if n_elements(*center[j,1-xy]) lt ny*.1 then ps=8 else ps=3
        foo = where( *yind[j,1-xy] lt center_range[j,1-xy,0] or $
                     *yind[j,1-xy] gt center_range[j,1-xy,1], count )
        case 1-xy of
          0:  color = purple()
          1:  color = yellow()
        endcase 
        if count gt 0 then oplot, (*center[j,1-xy])[foo], $
                                  (*yind[j,1-xy])[foo], co=color, ps=ps
        foo = where( *yind[j,1-xy] ge center_range[j,1-xy,0] and $
                     *yind[j,1-xy] le center_range[j,1-xy,1], count )
        case 1-xy of
          0:  color = red()
          1:  color = orange()
        endcase 
        if count gt 0 then oplot, (*center[j,1-xy])[foo], $
                                  (*yind[j,1-xy])[foo], co=color, ps=ps
      endif 
      paperplot3b:
      if keyword_set(paperplot) then begin
        if paperplot eq 1 or paperplot eq 3 then begin
          dcr = ( !x.crange[1] - !x.crange[0] )*.15
          if paperplot eq 3 then dcr = dcr/4
          foo = where( _yind ge center_range[j,xy,0] - dcr and $
                       _yind le center_range[j,xy,1] + dcr, count ) 
          oplot, _yind[foo], poly( _yind[foo], slopefit[j,xy,*] ), l=1
        endif else if paperplot eq 2 and ( j eq 1 or j eq 4 ) then begin
          dcr = ( !y.crange[1] - !y.crange[0] )*.15
          foo = where( *yind[j,1-xy] ge center_range[j,1-xy,0] - dcr and $
                       *yind[j,1-xy] le center_range[j,1-xy,1] + dcr, count )
          oplot, poly( center_range[j,1-xy,*] + [-1,1]*dcr, slopefit[j,0,*] ), $
                 center_range[j,1-xy,*] + [-1,1]*dcr, l=1
        endif
      endif else begin
        foo = where( _yind eq _yind )
        oplot, _yind[foo], poly( _yind[foo], slopefit[j,xy,*] ), l=2, co=green()
      endelse 
      if ( paperplot eq 1 and ppj eq 3 ) or paperplot eq 3 then begin
        if j lt j2 then begin
          j = j + 1
          goto, paperplot3b
        endif 
      endif 
      paperplot2c:
      yr=interpol( xv, indgen(szx), !y.crange )
      if xy eq 1 then yr = tkm(yr)
      if keyword_set(nopx) then begin
        axis, xaxis=1, /xs, xr=!x.crange, xtickn=notn
        axis, yaxis=0, /ys, yr=yr, /save, $
              ytit='Center '+(['Longitude (!Uo!N)','Radius'+tkmtit()])[xy]
        axis, yaxis=1, /ys, yr=!y.crange, ytickn=notn
        if paperplot eq 3 then goto, paperplot4
        axis, xaxis=0, /xs, xr=xr, /save, xtickn=notn
      endif else begin
        axis, xaxis=0, /xs, xr=!x.crange, xtickn=notn
              ;xtit=(['Radius (px)','Longitude (px)'])[xy]
        axis, xaxis=1, /xs, xr=!x.crange, xtickn=notn
        axis, yaxis=0, /ys, yr=!y.crange, ytit='Center (px)'
        axis, yaxis=1, /ys, yr=yr, /save, $
              ytit=(['Longitude (!Uo!N)','Radius'+tkmtit()])[xy]
      endelse 
      !p.multi[0] = !p.multi[0] - !p.multi[1] + 1

      if keyword_set(nowidth) then begin
        goto, nowidth1
      endif 
      if keyword_set(paperplot) then begin
        foo = where( _yind ge center_range[j,xy,0] and $
                     _yind le center_range[j,xy,1], count )
        ;yr = [min(_width[foo]-_width_sigma[foo],/nan)>0,$
        ;      max(_width[foo]+_width_sigma[foo],/nan)<szx]
        if ( paperplot eq 1 and ppj le 1 ) or $
           paperplot eq 2 or paperplot eq 3 then begin
          yr = [min(_width[foo],/nan)>0,$
                max(_width[foo],/nan)<szx]
        endif else begin
          yr = [min(_width,/nan)>0,$
                max(_width,/nan)<szx]
        endelse 
        q1 = 1 & q2 = 1
      endif else begin
        yr = [min(_width-_width_sigma,/nan)>0,$
              max(_width+_width_sigma,/nan)<szx]
        q1 = 0 & q2 = 2
      endelse 
      dyr = yr[1] - yr[0]
      yr = yr + [-.1,.1]*dyr
      plot, xrpx, $
            ;[min(_width,/nan),max(_width,/nan)], $
            yr, xs=5, ys=5, /nodata
      for q=q1,q2 do begin
        if paperplot eq 1 or paperplot eq 3 then begin
          if ppj le 1 then begin
            foo = where( _yind ge center_range[j,xy,0] and $
                         _yind le center_range[j,xy,1], count )
          endif else begin
            foo = where( _yind eq _yind, count )
          endelse 
        endif else case q of
          0: foo = where( _yind lt center_range[j,xy,0], count )
          1: foo = where( _yind ge center_range[j,xy,0] and $
                          _yind le center_range[j,xy,1], count )
          2: foo = where( _yind gt center_range[j,xy,1], count )
        endcase 
        if count gt 0 then begin
          if q eq 1 then case xy of
            0:  color = red()
            1:  color = orange()
          endcase 
          if q ne 1 then case xy of
            0:  color = purple()
            1:  color = yellow()
          endcase 
          if keyword_set(paperplot) then color = gray()
          polyfill, [_yind[foo],reverse(_yind[foo])], $
                    [_width[foo]-_width_sigma[foo],$
                     reverse(_width[foo]+_width_sigma[foo])], $
                    color=color, noclip=0
          oplot, _yind[foo], _width[foo]
        endif
      endfor
      if keyword_set(nopx) then begin
        axis, xaxis=0, /xs, xr=xr, /save, xtickn=notn
        axis, xaxis=1, /xs, xr=xr, xtickn=notn
        axis, yaxis=0, /ys, yr=!y.crange*dx, /save, ytit='Width (km)'
        axis, yaxis=1, /ys, yr=!y.crange, ytickn=notn
      endif else begin
        axis, xaxis=0, /xs, xr=!x.crange, xtickn=notn
              ;xtit=(['Radius (px)','Longitude (px)'])[xy]
        axis, xaxis=1, /xs, xr=!x.crange, xtickn=notn
        axis, yaxis=0, /ys, yr=!y.crange, ytit='Width (px)'
        axis, yaxis=1, /ys, yr=!y.crange*dx, /save, ytit='Width (km)'
      endelse 
      !p.multi[0] = !p.multi[0] - !p.multi[1] + 1
      nowidth1:

      if paperplot eq 1 then begin
        case ppj of
          0:  yr = [-.005,.037]
          1:  yr = [-.0005,.0035]
          2:  yr = [-.004,.025]
          3:  yr = [0,.00095]
          4:  yr = [-.005,.05]
        endcase 
        plot, xrpx, yr, xs=5, ys=5, /nodata
      endif else begin
        plot, xrpx, $
              ;[min([_ew,_ew_nobgsub]),max([_ew,_ew_nobgsub])], $
              [min([_ew]),max([_ew])], xs=5, ys=4, /nodata
      endelse
      polyfill, [_yind,reverse(_yind)], $
                [_ew-_ew_sigma,reverse(_ew+_ew_sigma)], $
                color=gray(), noclip=0
      ;!p.multi[0] = !p.multi[0] + 1
      ;plot, xrpx, $
      ;      [min([_ew,_ew_nobgsub]),max([_ew,_ew_nobgsub])], $
      ;      xtit='Line', ytit='Equivalent Width', /xs, /nodata
      oplot, _yind, _ew
      if xy eq 1 and not keyword_set(nopx) then begin
        for k=0,2 do oplot, cenbnd[k,j]*[1,1], !y.crange[0] + $
                            (!y.crange[1]-!y.crange[0])*[.05,.1], $
                            co=blue(), noclip=1
        oplot, cenbnd[1:2,j], !y.crange[0] + $
               (!y.crange[1]-!y.crange[0])*[.075,.075], co=blue(), noclip=1
      endif 
      if not keyword_set(paperplot) then oplot, _yind, _ew_nobgsub, l=1
      if keyword_set(nopx) then begin
        if keyword_set(paperplot) then begin
          paperplot4:
          if xr[1] - xr[0] gt 3 then xti = 1 else $
             if xr[1] - xr[0] gt 1 then xti = 0.4 else xti = 0.2
          axis, xaxis=0, xr=xr, /xs, /save, xticki=xti, $
                xtit=(['Radius'+tkmtit(),'Longitude (!Uo!N)'])[xy]
          if paperplot eq 3 then goto, paperplot_finish
        endif else begin
          axis, xaxis=0, xr=xr, /xs, /save, $
                xtit=(['Radius'+tkmtit(),'Longitude (!Uo!N)'])[xy]
        endelse 
        axis, xaxis=1, xr=!x.crange, /xs, xtickn=notn
        axis, yaxis=0, yr=!y.crange, /ys, ytit='I/F Equivalent Width (km)'
        axis, yaxis=1, yr=!y.crange*const, /ys, ytit='!Mt Equivalent Width (km)'
      endif else begin
        axis, xaxis=0, xr=!x.crange, /xs, $
              xtit=(['Radius (px)','Longitude (px)'])[xy]
        axis, xaxis=1, xr=!x.crange, /xs, xtickn=notn
        axis, yaxis=0, yr=!y.crange, /ys, ytit='I/F Equivalent Width (km)'
        axis, yaxis=1, yr=!y.crange*const, /ys, ytit='!Mt Equivalent Width (km)'
      endelse 
      if keyword_set(nowidth) then !p.multi[0] = !p.multi[0] - !p.multi[1]
      !p.multi[0] = !p.multi[0] + !p.multi[1]*(nrows-1)

      if paperplot eq 1 then begin
        ;help, ppj, j, j1, j2
        ppj = ppj + 1
        if ppj lt ppn then goto, paperplot0 else goto, paperplot_finish
      endif
      if paperplot eq 2 or paperplot eq 3 then begin
        ;help, ppj, j, j1, j2
        j = j + 1
        if j le j2 then goto, paperplot1c else goto, paperplot_finish
      endif
      wset, 10

    endif

  endfor

  query:
  reply = ''
  while reply eq '' do begin
    print, '[c]ontinue or [r]edo drawing or change s[m]ooth or change [f]ittype or change [s]queeze or select center_ran[g]e or select [l]ength/center or [q]uit?'
    read, reply
    case reply of
      'f':  begin
        reply2 = ''
        while reply2 eq '' do begin
          print, 'Use [c]onstant or [l]inear or [q]uadratic fit to each entire line, or local[ll]inear fit?'
          read, reply2
          case reply2 of
            'c':  _fituse = 1
            'l':  _fituse = 2
            'q':  _fituse = 3
            'll':  _fituse = 4
            else:  reply2 = ''
          endcase 
        endwhile
        reply2 = ''
        while reply2 eq '' do begin
          print, 'Apply this to [x] or [y] or [b]oth?'
          read, reply2
          case reply2 of
             'x':  fituse[j,0] = _fituse
             'y':  fituse[j,1] = _fituse
             'b':  fituse[j,*] = _fituse
             else:  reply2 = ''
          endcase 
        endwhile 
        !mouse.button = 4
        goto, plotline
      end 
      'l':  begin
        if keyword_set(nopx) then begin
          print, 'Please do not use this feature with nopx=1'
          reply = ''
        endif else begin
          reply2 = ''
          while reply2 eq '' do begin
            print, 'Enter center value (current: '+strtrim(cenbnd[0,j],2)+')'
            read, reply2
            if float(reply2) lt !x.crange[0] or $
               float(reply2) gt !x.crange[1] then begin
              print, 'Center must be in the x-range of the plot'
              reply2 = ''
            endif else cenbnd[0,j] = float(reply2)
          endwhile
          reply2 = ''
          while reply2 eq '' do begin
            print, 'Enter boundaries (current: ['+strtrim(cenbnd[1,j],2)+','+$
                                                  strtrim(cenbnd[2,j],2)+'])'
            read, reply2
            comma = strpos( reply2, ',' )
            if comma le 0 or comma eq strlen(reply2) then begin
              print, 'Please enter two values separated by a comma.'
              reply2 = ''
            endif else begin
              val1 = strmid(reply2,0,comma)
              val2 = strmid(reply2,comma+1,1000)
              if float(val1) ge float(val2) then begin
                print, 'Second value must be greater than first value.'
                reply2 = ''
              endif else if float(val1) lt !x.crange[0] then begin
                print, 'First value must be greater than '+$
                       strtrim(!x.crange[0]+1,2)
                reply2 = ''
              endif else if float(val2) gt !x.crange[1] then begin
                print, 'Second value must be less than '+$
                       strtrim(!x.crange[1]+1,2)
                reply2 = ''
              endif else begin
                cenbnd[1:2,j] = [ float(val1), float(val2) ]
              endelse 
            endelse 
          endwhile 
        endelse
        !mouse.button = 4
        goto, plotline
      end 
      's':  begin
        reply2 = ''
        while reply2 eq '' do begin
          print, 'Enter new value for squeeze (currentX=' + $
                 strtrim(squeeze[j,0],2) + ', currentY=' + $
                 strtrim(squeeze[j,1],2) + ', default=0.1)'
          read, reply2
          if float(reply2) lt 0 or float(reply2) ge 0.5 then begin
            print, 'squeeze must be at least 0 and less than 0.5'
            reply2 = ''
          endif else _squeeze = float(reply2)
        endwhile
        reply2 = ''
        while reply2 eq '' do begin
          print, 'Apply this to [x] or [y] or [b]oth?'
          read, reply2
          case reply2 of
             'x':  squeeze[j,0] = _squeeze
             'y':  squeeze[j,1] = _squeeze
             'b':  squeeze[j,*] = _squeeze
             else:  reply2 = ''
          endcase 
        endwhile 
        !mouse.button = 4
        goto, plotline
      end 
      'm':  begin
        reply2 = ''
        while reply2 eq '' do begin
          print, 'Enter new value for smooth (currentX=' + $
                 strtrim(sm[j,0],2) + ', currentY=' + $
                 strtrim(sm[j,1],2) + ', default=0)'
          read, reply2
          _sm = fix(reply2)
        endwhile
        reply2 = ''
        while reply2 eq '' do begin
          print, 'Apply this to [x] or [y] or [b]oth?'
          read, reply2
          case reply2 of
             'x':  sm[j,0] = _sm
             'y':  sm[j,1] = _sm
             'b':  sm[j,*] = _sm
             else:  reply2 = ''
          endcase 
        endwhile 
        !mouse.button = 4
        goto, plotline
      end 
      'g':  begin
        cr_redo:
        reply2 = ''
        while reply2 eq '' do begin
          print, 'Apply this to [x] or [y]?'
          read, reply2
          case reply2 of
             'x':  cri = 0
             'y':  cri = 1
             else:  reply2 = ''
          endcase 
        endwhile 
        reply2 = ''
        print, 'Currently center_range' + (['X','Y'])[cri] + '=[' + $
               strtrim(center_range[j,cri,0],2) + ',' + $
               strtrim(center_range[j,cri,1],2) + '].'
        while reply2 eq '' do begin
          print, 'Enter lower value of center_range' + (['X','Y'])[cri] + ':'
          read, reply2
          if float(reply2) lt 0 and float(reply2) gt sz[cri+1] then begin
            print, 'Values in center_range must be greater than ' + $
                   '0 and less than '+strtrim(sz[cri+1],2)
            reply2 = ''
          endif else if reply2 eq 'q' then retall else begin
            center_range[j,cri,0] = fix(reply2)
          endelse 
        endwhile 
        reply2 = ''
        while reply2 eq '' do begin
          print, 'Enter upper value of center_range' + (['X','Y'])[cri] + ':'
          read, reply2
          if float(reply2) lt center_range[j,cri,0] and $
             float(reply2) gt sz[cri+1] then begin
            print, 'Values in center_range must be greater than ' + $
                   'the lower value (' + strtrim(center_range[j,cri,0],2) + $
                   ') and less than '+strtrim(sz[cri+1],2)
            reply2 = ''
          endif else if reply2 eq 'q' then retall else begin
            center_range[j,cri,1] = fix(reply2)
          endelse 
        endwhile 
        reply2 = ''
        while reply2 eq '' do begin
          print, '[c]ontinue with these values or [r]edo?'
          read, reply2
          case reply2 of
            'c':  dummy = 1
            'q':  retall
            'r':  goto, cr_redo
            else:  reply2 = ''
          endcase 
        endwhile 
        !mouse.button = 4
        goto, plotline
      end 
      'r':  goto, redo
      'c':  dummy = 1
      'q':  retall
      else:  reply = ''
    endcase 
  endwhile
  save, yyy, nyyy, stmin, stmax, sm, sf, yind, cfit, lfit, qfit, $
        csigma, lsigma, qsigma, llsigma, llsm, cstatus, lstatus, qstatus, $
        fituse, squeeze, ew, ew_nobgsub, ew_sigma, ea, ea_sigma, $
        eatau, eatau_sigma, maxif, maxtau, cenbnd, $
        center, center_sigma, center_range, slopefit, slopefit_sigma, $
        theta, theta_sigma, width, width_sigma, _j1, _j2, filename=sfile

endfor
paperplot_finish:
if keyword_set(dolzr) then clzr

end
