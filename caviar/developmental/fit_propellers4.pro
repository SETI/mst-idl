; fit_propellers4.pro

; This code diverged from fit_propellers2.pro on 28 August 2007. 
; The difference is that the x-separation is measured between peak
; mid-lines rather than between peak edges.  system1=1 for the old way. 

; This code diverged from fit_propellers3.pro on 31 July 2009 with the
; addition of some new capabilities necessary to fully account for the 
; properties of giant propellers

if not keyword_set(prop_reproj) then restore, 'prop_reproj.sav'
npr = n_elements(prop_reproj)
if not keyword_set(j1) then j1 = 0l
if not keyword_set(j2) then j2 = npr - 1
sfile = 'fit_propellers4.sav'
if keyword_set(findfile(sfile)) then restore, sfile else begin
  _x1 = lonarr(npr)
  _x2 = lonarr(npr)
  _y1 = lonarr(npr)
  _y2 = lonarr(npr)
  _pp = fltarr( 8, npr )
  _pp_sigma = fltarr( 8, npr )
  _chisq = fltarr(npr)
  _dof = lonarr(npr)
  _flag = strarr(npr)
  _use = bytarr(npr) + 1
  _orphan = bytarr(npr)
  _initoffset = bytarr(npr)
  _subtractavg = bytarr(npr)
endelse
if keyword_set(shadow) then begin
  sfile_shadow = 'fit_propellers4_shadow.sav'
  if keyword_set(findfile(sfile_shadow)) then restore, sfile_shadow else begin
    shadowxy = dblarr(2,3,npr)
    shadowendxy = dblarr(2,npr)
    shadowpolar = dblarr(2,3,npr)
    shadowcart = dblarr(2,3,npr)
    shadowlen = dblarr(npr)
    shadowlenxy = dblarr(npr)
    shadowlen_sigma = dblarr(npr)
    shadowhh = dblarr(npr)
    shadowhh_sigma = dblarr(npr)
    shadow_absent = bytarr(npr)
  endelse
endif
if not keyword_exists(auto) then auto = 1
if not keyword_set(auto) then auto = 0
if not keyword_set(resfac) then resfac = 1;4
if not keyword_set(resfac1) then resfac1 = 4 ; display only

spawn, 'pwd', pwd
slashes = strsplit( pwd, '/' )
nslash = n_elements(slashes)
pwd = strmid( pwd, slashes[nslash-2], 1000 )

for j=j1,j2 do begin

  if keyword_set(goodonly) then begin
    if _use[j] eq 0 then begin
      while _use[j] eq 0 and j lt j2 do j=j+1
      if _use[j] eq 0 then goto, finish
    endif 
  endif 

  ; Unpack reprojected image and associated value, define grid arrays.
  mnrad = prop_reproj[j].radlon[0]
  mxrad = prop_reproj[j].radlon[1]
  mnlon = prop_reproj[j].radlon[2]
  mxlon = prop_reproj[j].radlon[3]
  rrpi = *( prop_reproj[j].rrpi )
  rrpi_orig = rrpi
  sz = size(rrpi)
  ;longrid = dindgen(sz[1]+1) / sz[1] * (mxlon-mnlon) + mnlon
  ;radgrid = dindgen(sz[2]+1) / sz[2] * (mxrad-mnrad) + mnrad
  ;lonx = rebin( [ [longrid[0:sz[1]-1]], [longrid[1:sz[1]]] ], sz[1], 1 )
  ;radx = rebin( [ [radgrid[0:sz[2]-1]], [radgrid[1:sz[2]]] ], sz[2], 1 )
  radx = make_radi( mnrad, mxrad, mnlon, mxlon, sz, loni=lonx )
  sgldbl = 0

  ; If the edge of the image seems to be included, then set contaminated
  ; values equal to image median to avoid confusing the fitting routine.
  foo = wher( rrpi eq 0, count )
  mask = bytarr( sz[1], sz[2] ) + 1
  if count gt 10 then begin
    print, 'rrpi contains zeroes, so assume it goes off the edge of the image.'
    box = 1;5
    ;if prop_reproj[j].xy[2] gt 850 then box = 0
    for k=0l,count-1 do begin
      mask[ (foo[0,k]-box)>0 : (foo[0,k]+box)<(sz[1]-1), $
            (foo[1,k]-box)>0 : (foo[1,k]+box)<(sz[2]-1) ] = 0
    endfor
    rrpi[where( mask eq 0 )] = median(rrpi[where( mask eq 1 )])
  endif

  ; Create a window and show the image
  replot: 
  if keyword_set(bksub) then begin
    dot = rstrpos( images[prop_reproj[j].images], '.' )
    restore, strmid( images[prop_reproj[j].images], 0, dot )+'.bksub'
    window, 1, xs=1024, ys=1024
    tvscl, /order, bksub_img > (-2*stddev(bksub_img)) < 2*stddev(bksub_img)
    plots, prop_reproj[j].xy[3]+[-4,4,4,-4,-4], $
           1023-prop_reproj[j].xy[2]+[-4,-4,4,4,-4], color=ctyellow(), /device
  endif 
  if keyword_set(_subtractavg[j]) then begin
    rrpi = fit_propellers4_subtractavg( rrpi, rrpi_orig, mask, sz )
  endif else rrpi = rrpi_orig
  rrpi0 = rrpi
  sz0 = size(rrpi)
  rrpi0 = rebin( rrpi0, sz0[1]*resfac1, sz0[2]*resfac1, /sample )
  sz0[1:2] = sz0[1:2] * resfac1
  hthresh = 0;5
  run_histogram, rrpi0, stmin, stmax, threshold=hthresh, /nocrop, $
                 uselog=0, /silent
  window, 10, xs=sz0[1], ys=sz0[2]
  plotalso = -1
  if keyword_set(prop_reproj[j].primary) then begin
    if keyword_set(prop_reproj[j].duplicates) then begin
      foo = (where( prop_reproj.prop eq prop_reproj[j].duplicates, count ))[0]
      if count ne 1 then print, 'There are actually '+strtrim(count,2)+' duplicates';stop
      plotalso = foo
      primsec = 'primary, corresponds with #' + strtrim( plotalso, 2 )
    endif else begin
      foo = where( prop_reproj.prop eq prop_reproj[j].prop, count )
      if count gt 2 then print, 'There are actually '+strtrim(count,2)+' duplicates';stop
      if count eq 1 then primsec = 'primary' else begin
        plotalso = (foo[where( foo ne j )])[0]
        if keyword_set(_orphan[plotalso]) then begin
          primsec = 'primary, corresponding #' + strtrim( plotalso, 2 ) + $
                    ' has been orphaned'
          plotalso = -1
        endif else begin
          primsec = 'primary, corresponds with #' + strtrim( plotalso, 2 )
        endelse
      endelse
    endelse
  endif else begin
    foo = where( prop_reproj.prop eq prop_reproj[j].prop, count )
    if count ne 2 then print, 'There are actually '+strtrim(count,2)+' duplicates';stop
    plotalso = (foo[where( foo ne j )])[0]
    primsec = 'secondary, corresponds with #' + strtrim( plotalso, 2 )
    if keyword_set(_orphan[j]) then primsec = primsec + ' (orphaned)'
  endelse
  if plotalso ne -1 then begin
    rrpi1 = *( prop_reproj[plotalso].rrpi )
    sz1 = size(rrpi1)
    rrpi1 = rebin( rrpi1, sz1[1]*resfac1, sz1[2]*resfac1, /sample )
    sz1[1:2] = sz1[1:2] * resfac1
    run_histogram, rrpi1, stmin1, stmax1, threshold=hthresh, /nocrop, $
                   uselog=0, /silent
    window, 12, xs=sz1[1], ys=sz1[2], xpos=0, ypos=100;, xpos=sz0[1]*1.5
    tvscl, rrpi1>stmin1<stmax1
    if _x1[plotalso] ne 0 then begin
      plots, [ _x1[plotalso], _x2[plotalso]+1, _x2[plotalso]+1, $
               _x1[plotalso], _x1[plotalso] ]*resfac1, $
             [ _y1[plotalso], _y1[plotalso], _y2[plotalso]+1, $
               _y2[plotalso]+1, _y1[plotalso] ]*resfac1, $
             color=ctblue(), /dev
    endif
    if _pp[7,plotalso] eq 0 then begin
      if _pp[4,plotalso] ne 0 then begin
        ppp = _pp[*,plotalso] * resfac1
        if _use[plotalso] then fitcolor=ctgreen() else fitcolor=ctpurple()
        if keyword_set(fwhm) then ppp[2:3] = ppp[2:3] * sqrt(2*alog(2))
        plots, lindgen(2*ppp[2]>1) + ppp[4]-ppp[2], ppp[5] + ppp[3]* $
               sqrt( 1 - (( lindgen(2*ppp[2]>1) - ppp[2] )/ppp[2])^2 ), $
               /device, color=fitcolor
        plots, lindgen(2*ppp[2]>1) + ppp[4]-ppp[2], ppp[5] - ppp[3]* $
               sqrt( 1 - (( lindgen(2*ppp[2]>1) - ppp[2] )/ppp[2])^2 ), $
               /device, color=fitcolor
      endif
    endif else begin
      for k=-1,1,2 do begin
        ppp = _pp[*,plotalso] * resfac1
        if _use[plotalso] then fitcolor=ctgreen() else fitcolor=ctpurple()
        if keyword_set(system1) then begin
          p1 = [ ppp[0:3], ppp[4]+k*ppp[6]+k*ppp[2], ppp[5]-k*ppp[7], 0, 0 ]
        endif else begin
          p1 = [ ppp[0:3], ppp[4]+k*ppp[6], ppp[5]-k*ppp[7], 0, 0 ]
        endelse 
        if keyword_set(fwhm) then p1[2:3] = p1[2:3] * sqrt(2*alog(2))
        plots, lindgen(2*p1[2]) + p1[4]-p1[2], p1[5] + p1[3]* $
               sqrt( 1 - (( lindgen(2*p1[2]) - p1[2] )/p1[2])^2 ), $
               /device, color=fitcolor
        plots, lindgen(2*p1[2]) + p1[4]-p1[2], p1[5] - p1[3]* $
               sqrt( 1 - (( lindgen(2*p1[2]) - p1[2] )/p1[2])^2 ), $
               /device, color=fitcolor
      endfor
    endelse
    wset, 10
  endif
  primsec = primsec + '  ---  '
  if _use[j] eq 0 then primsec = primsec + 'DON''T '
  primsec = primsec + 'USE'
  print, '--------------'
  print, 'Propeller #'+strtrim(j,2)+'  ---  '+primsec
  print, '--------------'

  ; Define sub-image boundaries (default is entire image)
  x1 = 0 & x2 = sz[1]-1 & y1 = 0 & y2 = sz[2]-1
  bluebox = 0
  if keyword_set(_x1[j]) then begin
    deselect:
    x1 = _x1[j] & x2 = _x2[j] & y1 = _y1[j] & y2 = _y2[j]
    bluebox = 1
  endif
  if 4 eq 5 then begin
    select:
    print, 'Click on lower-left corner of area to select.'
    cursor, x1, y1, 3, /device
    x1 = long( x1 / resfac1 )
    y1 = long( y1 / resfac1 )
    _x1[j] = x1 & _y1[j] = y1
    print, 'Click on upper-right corner of area to select.'
    cursor, x2, y2, 3, /device
    x2 = ceil( x2 / resfac1 )
    y2 = ceil( y2 / resfac1 )
    _x2[j] = x2 & _y2[j] = y2
    bluebox = 1
  endif

  ; Fit single- or double-gaussian
  refit: 
  if not keyword_set(datamodel) then datamodel = 1
  case datamodel of
    1: tvscl, rrpi0>stmin<stmax
    2: begin
      xx = rebin( lindgen(sz[1]), sz[1], sz[2] )
      yy = rebin( lindgen(1,sz[2]), sz[1], sz[2] )
      if keyword_set(plotguess) then begin
        pp_plot = pp_est + [ 0, 0, 0, 0, x1, y1, 0, 0 ]
      endif else pp_plot = pp
      if keyword_set(system1) then begin
        zfit1 = mpfit2dpeak_propeller1( xx, yy, pp_plot )
      endif else begin
        zfit1 = mpfit2dpeak_propeller( xx, yy, pp_plot )
      endelse 
      zfit1 = rebin( zfit1, sz[1]*resfac1, sz[2]*resfac1, /sample )
      tvscl, zfit1
    end
  endcase
  if keyword_set(bluebox) then begin
    plots, [ x1, x2+1, x2+1, x1, x1 ]*resfac1, $
           [ y1, y1, y2+1, y2+1, y1 ]*resfac1, color=ctblue(), /dev
  endif
  if keyword_set(shadow) then begin
    if not keyword_set(findfile('fit_propeller_shadow.sav')) then begin
      stop, 'fit_propeller_shadow.sav not found'
    endif else restore, 'fit_propeller_shadow.sav'
    shadowxy[*,*,j] = [ interpol( findgen(sz[1]), lonx, shadow[j,*,1] ), $
                        interpol( findgen(sz[2]), radx, shadow[j,*,2] ) ]
    if keyword_set(_pp[4,j]) then begin
      shadowxy[0,*,j] = shadowxy[0,*,j] - shadowxy[0,1,j] + _pp[4,j]
      shadowxy[1,*,j] = shadowxy[1,*,j] - shadowxy[1,1,j] + _pp[5,j]
    endif 
    plots, shadowxy[0,*,j]*resfac1, shadowxy[1,*,j]*resfac1, $
           color=ctyellow(), /dev
  endif 
  if not keyword_set(sgldbl) then begin
    if _pp[7,j] eq 0 then sgldbl = 1 else sgldbl = 2
  endif
  if sgldbl eq 1 then begin
    if _pp[4,j] eq 0 then begin
      weights = fltarr( sz[1], sz[2] ) + 1.0/stddev(rrpi)^2
      zfit = mpfit2dpeak( rrpi[x1:x2,y1:y2], pp1, sigma=pp_sigma, $
                          chisq=chisq, weights=weights[x1:x2,y1:y2], /quiet )
      pp = pp1 + [ 0, 0, 0, 0, x1, y1, 0 ]
      dof = n_elements(zfit)/resfac^2 - n_elements(pp)
      pp = [ pp, 0 ]
      pp_sigma = [ pp_sigma, 0 ]
    endif else begin
      pp = _pp[*,j]
      pp_sigma = _pp_sigma[*,j]
      chisq = _chisq[j]
      dof = _dof[j]
    endelse
    ppp = pp * resfac1
    if _use[j] then fitcolor=ctgreen() else fitcolor=ctpurple()
    if keyword_set(fwhm) then ppp[2:3] = ppp[2:3] * sqrt(2*alog(2))
    if n_elements(prop_reproj[j].xy) eq 6 then begin
      plots, prop_reproj[j].xy[4]*resfac1, prop_reproj[j].xy[5]*resfac1, $
             color=fitcolor, ps=1, /device
    endif 
    plots, lindgen((2*ppp[2])>1) + ppp[4]-ppp[2], ppp[5] + ppp[3]* $
           sqrt( 1 - (( lindgen((2*ppp[2])>1) - ppp[2] )/ppp[2])^2 ), $
           /device, color=fitcolor
    plots, lindgen((2*ppp[2])>1) + ppp[4]-ppp[2], ppp[5] - ppp[3]* $
           sqrt( 1 - (( lindgen((2*ppp[2])>1) - ppp[2] )/ppp[2])^2 ), $
           /device, color=fitcolor
    print, '   bg value    peak value       width-X      width-Y     '+$
           'center-X     center-Y       tilt'
    print, pp
    print, pp_sigma * sqrt( chisq / dof )
    ;print, chisq
  endif else if sgldbl eq 2 then begin
    if _pp[4,j] eq 0 then begin
      if keyword_set(system1) then begin
        if pwd eq 'SOI/SOISPTURN' then begin
          pp_est = [ mean(rrpi[x1:x2,y1:y2]), max(rrpi[x1:x2,y1:y2]), $
                     15.5*resfac, 3*resfac, sz[1]/2-x1, sz[2]/2-y1, $
                     7.5*resfac, 2.5*resfac ]
        endif else begin
          pp_est = [ mean(rrpi[x1:x2,y1:y2]), max(rrpi[x1:x2,y1:y2]), $
                     sz[1]/15, 1*resfac, sz[1]/2-x1, sz[2]/2-y1, sz[1]/30, 0 ]
        endelse 
      endif else begin
        if keyword_set(x1) then begin
          pp_est = [ mean(rrpi[x1:x2,y1:y2]), max(rrpi[x1:x2,y1:y2]), $
                     (x2-x1-1)/8., 1*resfac, (x2-x1-1)/2., (y2-y1-1)/2., $
                     (x2-x1-1)/4., 0 ]
        endif else if pwd eq 'SOI/SOISPTURN' then begin
          pp_est = [ mean(rrpi[x1:x2,y1:y2]), max(rrpi[x1:x2,y1:y2]), $
                     15.5*resfac, 3*resfac, sz[1]/2-x1, sz[2]/2-y1, $
                     23*resfac, 2.5*resfac ]
        endif else if pwd eq '035/AZSCNLOPH' then begin
          pp_est = [ mean(rrpi[x1:x2,y1:y2]), max(rrpi[x1:x2,y1:y2]), $
                     sz[1]/30, 1*resfac, sz[1]/2-x1, sz[2]/2-y1, sz[1]/20, 0 ]
        endif else begin
          pp_est = [ mean(rrpi[x1:x2,y1:y2]), max(rrpi[x1:x2,y1:y2]), $
                     sz[1]/15, 1*resfac, sz[1]/2-x1, sz[2]/2-y1, sz[1]/10, 0 ]
        endelse 
      endelse 
      xx = rebin( lindgen(sz[1]), sz[1], sz[2] )
      yy = rebin( lindgen(1,sz[2]), sz[1], sz[2] )
      if keyword_set(system1) then begin
        zfit_est = mpfit2dpeak_propeller1( xx, yy, pp_est )
      endif else begin
        zfit_est = mpfit2dpeak_propeller( xx, yy, pp_est )
      endelse 
      zfit_est = rebin( zfit_est, sz[1]*resfac1, sz[2]*resfac1, /sample )
      if keyword_set(_initoffset[j]) then pp_est[7] = pp_est[7] + 1*resfac
      weights = fltarr( sz[1], sz[2] ) + 1.0/stddev(rrpi)^2
pp_est = double(pp_est)
      if keyword_set(system1) then begin
        zfit = mpfit_propeller1( rrpi[x1:x2,y1:y2], pp1, sigma=pp_sigma, $
                                 chisq=chisq, /quiet, estimate=pp_est, /pos, $
                                 covar=covar, weights=weights[x1:x2,y1:y2] )
      endif else begin
        zfit = mpfit_propeller( rrpi[x1:x2,y1:y2], pp1, sigma=pp_sigma, $
                                chisq=chisq, /quiet, estimate=pp_est, /pos, $
                                covar=covar, weights=weights[x1:x2,y1:y2] )
      endelse 
      pp = pp1 + [ 0, 0, 0, 0, x1, y1, 0, 0 ]
      dof = n_elements(zfit)/resfac^2 - n_elements(pp)
    endif else begin
      pp = _pp[*,j]
      pp_sigma = _pp_sigma[*,j]
      chisq = _chisq[j]
      dof = _dof[j]
    endelse
    for k=-1,1,2 do begin
      ppp = pp * resfac1
      if _use[j] then fitcolor=ctgreen() else fitcolor=ctpurple()
      if keyword_set(system1) then begin
        p1 = [ ppp[0:3], ppp[4]+k*ppp[6]+k*ppp[2], ppp[5]-k*ppp[7], 0, 0 ]
      endif else begin
        p1 = [ ppp[0:3], ppp[4]+k*ppp[6], ppp[5]-k*ppp[7], 0, 0 ]
      endelse 
      if keyword_set(fwhm) then p1[2:3] = p1[2:3] * sqrt(2*alog(2))
      plots, lindgen((2*p1[2])>1) + p1[4]-p1[2], p1[5] + p1[3]* $
             sqrt( 1 - (( lindgen((2*p1[2])>1) - p1[2] )/p1[2])^2 ), $
             /device, color=fitcolor
      plots, lindgen((2*p1[2])>1) + p1[4]-p1[2], p1[5] - p1[3]* $
             sqrt( 1 - (( lindgen((2*p1[2])>1) - p1[2] )/p1[2])^2 ), $
             /device, color=fitcolor
    endfor
    print, '   bg value    peak value       width-X      width-Y     '+$
           'center-X     center-Y   separation-X separation-Y'
    print, pp
    print, pp_sigma * sqrt( chisq / dof )
    ;print, chisq
  endif else stop, 'sgldbl must be 1 or 2.'
  print, strmid( images[prop_reproj[j].images], 0, 11 ) + ':  [' + $
         strtrim(prop_reproj[j].xy[2],2) + ',' + $
         strtrim(prop_reproj[j].xy[3],2) + ']'

  reply = ''
  if keyword_set(shadow) then begin
    if shadowendxy[0,j] eq 0 and not keyword_set(shadow_absent[j]) then begin
      print, 'Click at the end of the shadow.  The yellow line indicates '+$
             'the direction of sunlight.'
      cursor, x, y, 3, /device
      x = float(x)/resfac1
      y = float(y)/resfac1
      shadowvec = [ shadowxy[0,2,j]-shadowxy[0,1,j], $
                    shadowxy[1,2,j]-shadowxy[1,1,j] ]
      shadowvecperp = [ -shadowvec[1], shadowvec[0] ]
      fac2 = ( y - shadowxy[1,1,j] + $
               shadowvecperp[1]/shadowvecperp[0]*( shadowxy[0,1,j] - x ) )/$
             ( shadowvec[1] - shadowvecperp[1]/shadowvecperp[0]*shadowvec[0] )
      shadowendxy[*,j] = [ shadowxy[0,1,j] + shadowvec[0]*fac2, $
                           shadowxy[1,1,j] + shadowvec[1]*fac2 ]
    endif 
    solid_circles
    plots, shadowendxy[0,j]*resfac1, shadowendxy[1,j]*resfac1, $
           ps=8, color=ctyellow(), /dev
    shadowlenxy[j] = sqrt(total( (shadowxy[*,1,j]-shadowendxy[*,j])^2 ))
    shadowpolar[*,*,j] = $
                 [[0.0d0,0], $
                  [ interpol( lonx, findgen(sz[1]), shadowendxy[0,j] ), $
                    interpol( lonx, findgen(sz[1]), shadowxy[0,1,j] ) ], $
                  [ interpol( radx, findgen(sz[2]), shadowendxy[1,j] ), $
                    interpol( radx, findgen(sz[2]), shadowxy[1,1,j] ) ]]
    shadowpolar[*,1,j] = shadowpolar[*,1,j] * !dpi/180
    shadowcart[*,*,j] = polar_to_cart( shadowpolar[*,*,j] )
    shadowlen[j] = v_mag( shadowcart[1,*,j] - shadowcart[0,*,j] )
    shadowlen_sigma[j] = shadowlen[j] / shadowlenxy[j] ; assuming 1 pixel error
    geomfac = tan(abs(90-incidence[j])*!dpi/180)
    shadowhh[j] = shadowlen[j] * geomfac
    shadowhh_sigma[j] = shadowlen_sigma[j] * geomfac
    print, 'Shadow length:  '+strtrim(shadowlen[j],2)+' +- '+$
           strtrim(shadowlen_sigma[j],2)+' km'
    print, 'Implied Obstacle Height:  '+strtrim(shadowhh[j]*1e3,2)+' +- '+$
           strtrim(shadowhh_sigma[j]*1e3,2)+' m'
    while reply eq '' do begin
      if keyword_set(shadow_absent[j]) then shadabstxt = ' not' else begin
        shadabstxt = ''
      endelse 
      print, 'Proceed with this location for the end of the shadow?'
      print, '[y]es, [n]o, [q]uit, shadow'+shadabstxt+' [a]bsent'
      read, reply
      case reply of
        'a': begin
          shadow_absent[j] = 1-shadow_absent[j]
          shadowendxy[*,j] = 0
          shadowpolar[*,*,j] = 0
          shadowcart[*,*,j] = 0
          shadowlen[j] = 0
          shadowlenxy[j] = 0
          shadowlen_sigma[j] = 0
          shadowhh[j] = 0
          shadowhh_sigma[j] = 0
          if not keyword_set(shadow_absent[j]) then goto, refit
        end
        'n': begin
          shadowendxy[*,j] = 0
          goto, refit
        end 
        'q': retall
        'y': begin
        end 
        else: reply = ''
      endcase 
    endwhile 
  endif else while reply eq '' do begin
    case sgldbl of
      1: sgldbltxt = 's[w]itch to double-gaussian fit'
      2: sgldbltxt = 's[w]itch to single-gaussian fit'
    endcase
    case datamodel of 
      1: datamodeltxt = '[p]lot model'
      2: datamodeltxt = '[p]lot data'
    endcase
    case _use[j] of
      0: usetxt = '[u]se this feature'
      1: usetxt = 'don''t [u]se this feature'
    endcase
    case _initoffset[j] of
      0: inofftxt = 'use an [i]nitial offset'
      1: inofftxt = 'remove [i]nitial offset'
    endcase
    case _subtractavg[j] of
      0: subtxt = ''
      1: subtxt = 'do not '
    endcase 
    case _orphan[j] of
      0: orphantxt = '[o]rphan this feature'
      1: orphantxt = 'de-[o]rphan this feature'
    endcase
    if plotalso eq -1 then begin
      if keyword_set(_flag[j]) then begin
        print, '--------------'
        print, 'COMMENT: '+_flag[j]
        print, '--------------'
      endif
    endif else begin
      if keyword_set(_flag[j]) or keyword_set(_flag[plotalso]) then begin
        print, '--------------'
        if keyword_set(_flag[j]) then begin
          print, 'COMMENT #'+strtrim(j,2)+': '+_flag[j]
        endif
        if keyword_set(_flag[plotalso]) then begin
          print, 'COMMENT #'+strtrim(plotalso,2)+': '+_flag[plotalso]
        endif
        print, '--------------'
      endif
    endelse
    print, '[s]elect a sub-image / [d]e-select sub-image / '+sgldbltxt+$
           ' / '+datamodeltxt+' / plot [g]uess / [f]lag with a comment / '+$
           usetxt+' / [r]edo fit / '+orphantxt+$
           ' / '+inofftxt+' / '+subtxt+'su[b]tract average radial profile / '+$
           '[c]ontinue / [q]uit / sa[v]e and quit'
    read, reply
    case reply of
      's': goto, select
      'd': begin
        _x1[j] = 0 & _x2[j] = sz[1]-1 & _y1[j] = 0 & _y2[j] = sz[2]-1
        goto, deselect
      end
      'w': begin
        _pp[*,j] = 0
        sgldbl = 3 - sgldbl
        datamodel = 1
        if sgldbl eq 1 then plotguess = 0
        goto, refit
      end
      'p': begin
        datamodel = 3 - datamodel
        plotguess = 0
        goto, refit
      end
      'g': begin
        datamodel = 2
        if sgldbl eq 2 then plotguess = 1 else begin
          print, 'No guess to plot for single-gaussian.  Plotting model...'
          plotguess = 0
        endelse 
        goto, refit
      end 
      'f': begin
        if keyword_set(_flag[j]) then print, 'Current comment: '+_flag[j]
        print, 'Type comment:'
        flag = ''
        read, flag
        _flag[j] = flag
        reply = ''
      end
      'u': begin
        _use[j] = 1 - _use[j]
; This is not a good idea now that you often have more than two propellers
; linked together, but only one of them is plotalso.  Toggle "use" independently
;        if plotalso ne -1 then _use[plotalso] = _use[j]
        reply = ''
      end
      'i': begin
        _initoffset[j] = 1 - _initoffset[j]
        goto, refit
      end
      'b': begin
        _subtractavg[j] = 1 - _subtractavg[j]
        goto, replot
      end 
      'r': begin
        _pp[*,j] = 0
        goto, refit
      end
      'o': begin
        _orphan[j] = 1 - _orphan[j]
        if _orphan[j] eq 1 then begin
          print, 'This feature will no longer appear as linked with #'+$
                 strtrim(plotalso,2)
        endif else begin
          print, 'This feature will once again appear as linked with #'+$
                 strtrim(plotalso,2)
        endelse
        reply = ''
      end
      'c': reply = 'c'
      'q': retall
      'v': begin
        quit = 1
      end
      else: reply = ''
    endcase
  endwhile

  finish:
  datamodel = 1
  sgldbl = 1
  _pp[*,j] = pp
  _pp_sigma[*,j] = pp_sigma
  _chisq[j] = chisq
  _dof[j] = dof
  if keyword_set(shadow) then begin
    save, shadowxy, shadowendxy, shadowpolar, shadowcart, shadowlen, $
          shadowlenxy, shadowlen_sigma, shadowhh, shadowhh_sigma, $
          shadow_absent, filename=sfile_shadow
  endif else begin
    save, _x1, _x2, _y1, _y2, _pp, _pp_sigma, _chisq, _dof, _flag, _use, $
          _orphan, _initoffset, _subtractavg, filename=sfile
  endelse 
  while !d.window ne -1 do wdelete, !d.window
  if keyword_set(quit) then begin
    quit = 0
    retall
  endif

endfor

end
