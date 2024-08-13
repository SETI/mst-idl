if not keyword_set(prop_reproj) then restore, 'prop_reproj.sav.4'
npr = n_elements(prop_reproj)
if not keyword_set(j1) then j1 = 0l
if not keyword_set(j2) then j2 = npr - 1
if keyword_set(findfile('fit_propellers.sav.4')) then begin
  restore, 'fit_propellers.sav.4'
endif else begin
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
endelse
if not keyword_exists(auto) then auto = 1
if not keyword_set(auto) then auto = 0
if not keyword_set(resfac) then resfac = 1;4

for j=j1,j2 do begin

  ; Unpack reprojected image and associated value, define grid arrays.
  mnrad = prop_reproj[j].radlon[0]
  mxrad = prop_reproj[j].radlon[1]
  mnlon = prop_reproj[j].radlon[2]
  mxlon = prop_reproj[j].radlon[3]
  rrpi = *( prop_reproj[j].rrpi )
  sz = size(rrpi)
  longrid = dindgen(sz[1]+1) / sz[1] * (mxlon-mnlon) + mnlon
  radgrid = dindgen(sz[2]+1) / sz[2] * (mxrad-mnrad) + mnrad
  lonx = rebin( [ [longrid[0:sz[1]-1]], [longrid[1:sz[1]]] ], sz[1], 1 )
  radx = rebin( [ [radgrid[0:sz[2]-1]], [radgrid[1:sz[2]]] ], sz[2], 1 )
  sgldbl = 0

  ; If the edge of the image seems to be included, then set contaminated
  ; values equal to image median to avoid confusing the fitting routine.
  foo = wher( rrpi eq 0, count )
  if count gt 10 then begin
    print, 'rrpi contains zeroes, so assume it goes off the edge of the image.'
    mask = bytarr( sz[1], sz[2] ) + 1
    box = 5
    if prop_reproj[j].xy[2] gt 850 then box = 0
    for k=0,count-1 do begin
      mask[ (foo[0,k]-box)>0 : (foo[0,k]+box)<(sz[1]-1), $
            (foo[1,k]-box)>0 : (foo[1,k]+box)<(sz[2]-1) ] = 0
    endfor
    rrpi[where( mask eq 0 )] = median(rrpi[where( mask eq 1 )])
  endif

  ; Create a window and show the image
  window, 10, xs=sz[1], ys=sz[2], ypos=0
  hthresh = 0;5
  run_histogram, rrpi, stmin, stmax, threshold=hthresh, /nocrop, /silent
  plotalso = -1
  if keyword_set(prop_reproj[j].primary) then begin
    if keyword_set(prop_reproj[j].duplicates) then begin
      foo = (where( prop_reproj.prop eq prop_reproj[j].duplicates, count ))[0]
      if count ne 1 then stop
      plotalso = foo
      primsec = 'primary, corresponds with #' + strtrim( plotalso, 2 )
    endif else begin
      foo = where( prop_reproj.prop eq prop_reproj[j].prop, count )
      if count gt 2 then stop
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
    if count ne 2 then stop
    plotalso = (foo[where( foo ne j )])[0]
    primsec = 'secondary, corresponds with #' + strtrim( plotalso, 2 )
    if keyword_set(_orphan[j]) then primsec = primsec + ' (orphaned)'
  endelse
  if plotalso ne -1 then begin
    rrpi1 = *( prop_reproj[plotalso].rrpi )
    sz1 = size(rrpi)
    window, 12, xs=sz1[1], ys=sz1[2], xpos=sz[1]*1.5, ypos=0
    run_histogram, rrpi1, stmin1, stmax1, threshold=hthresh, /nocrop, /silent
    tvscl, rrpi1>stmin1<stmax1
    if _x1[plotalso] ne 0 then begin
      plots, [ _x1[plotalso], _x2[plotalso], _x2[plotalso], $
               _x1[plotalso], _x1[plotalso] ], $
             [ _y1[plotalso], _y1[plotalso], _y2[plotalso], $
               _y2[plotalso], _y1[plotalso] ], $
             color=ctblue(), /dev
    endif
    if _pp[7,plotalso] eq 0 then begin
      if _pp[0,plotalso] ne 0 then begin
        ppp = _pp[*,plotalso]
        if keyword_set(fwhm) then ppp[2:3] = ppp[2:3] * sqrt(2*alog(2))
        plots, lindgen(2*ppp[2]) + ppp[4]-ppp[2], ppp[5] + ppp[3]* $
               sqrt( 1 - (( lindgen(2*ppp[2]) - ppp[2] )/ppp[2])^2 ), $
               /device, color=ctpurple()
        plots, lindgen(2*ppp[2]) + ppp[4]-ppp[2], ppp[5] - ppp[3]* $
               sqrt( 1 - (( lindgen(2*ppp[2]) - ppp[2] )/ppp[2])^2 ), $
               /device, color=ctpurple()
      endif
    endif else begin
      for k=-1,1,2 do begin
        ppp = _pp[*,plotalso]
        p1 = [ ppp[0:3], ppp[4]+k*ppp[6]+k*ppp[2], ppp[5]-k*ppp[7], 0, 0 ]
        if keyword_set(fwhm) then p1[2:3] = p1[2:3] * sqrt(2*alog(2))
        plots, lindgen(2*p1[2]) + p1[4]-p1[2], p1[5] + p1[3]* $
               sqrt( 1 - (( lindgen(2*p1[2]) - p1[2] )/p1[2])^2 ), $
               /device, color=ctpurple()
        plots, lindgen(2*p1[2]) + p1[4]-p1[2], p1[5] - p1[3]* $
               sqrt( 1 - (( lindgen(2*p1[2]) - p1[2] )/p1[2])^2 ), $
               /device, color=ctpurple()
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
    _x1[j] = x1 & _y1[j] = y1
    print, 'Click on upper-right corner of area to select.'
    cursor, x2, y2, 3, /device
    _x2[j] = x2 & _y2[j] = y2
    bluebox = 1
  endif

  ; Fit single- or double-gaussian
  refit: 
  if not keyword_set(datamodel) then datamodel = 1
  case datamodel of
    1: tvscl, rrpi>stmin<stmax
    2: begin
      xx = rebin( lindgen(sz[1]), sz[1], sz[2] )
      yy = rebin( lindgen(1,sz[2]), sz[1], sz[2] )
      zfit1 = mpfit2dpeak_propeller( xx, yy, pp )
      tvscl, zfit1
    end
  endcase
  if keyword_set(bluebox) then begin
    plots, [ x1, x2, x2, x1, x1 ], [ y1, y1, y2, y2, y1 ], color=ctblue(), /dev
  endif
  if not keyword_set(sgldbl) then begin
    if _pp[7,j] eq 0 then sgldbl = 1 else sgldbl = 2
  endif
  if sgldbl eq 1 then begin
    if _pp[0,j] eq 0 then begin
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
    ppp = pp
    if keyword_set(fwhm) then ppp[2:3] = ppp[2:3] * sqrt(2*alog(2))
    plots, lindgen((2*ppp[2])>1) + ppp[4]-ppp[2], ppp[5] + ppp[3]* $
           sqrt( 1 - (( lindgen((2*ppp[2])>1) - ppp[2] )/ppp[2])^2 ), $
           /device, color=ctpurple()
    plots, lindgen((2*ppp[2])>1) + ppp[4]-ppp[2], ppp[5] - ppp[3]* $
           sqrt( 1 - (( lindgen((2*ppp[2])>1) - ppp[2] )/ppp[2])^2 ), $
           /device, color=ctpurple()
    print, '   bg value    peak value       width-X      width-Y     '+$
           'center-X     center-Y       tilt'
    print, pp
    print, pp_sigma * sqrt( chisq / dof )
    ;print, chisq
  endif else if sgldbl eq 2 then begin
    if _pp[0,j] eq 0 then begin
      pp_est = [ mean(rrpi[x1:x2,y1:y2]), max(rrpi[x1:x2,y1:y2]), $
;                 sz[1]/15, 1*resfac, sz[1]/2-x1, sz[2]/2-y1, 0, 1*resfac ]
                 sz[1]/15, 1*resfac, sz[1]/2-x1, sz[2]/2-y1, 0, 0 ]
      ;; SOI/SOISPTURN
      ;pp_est = [ mean(rrpi[x1:x2,y1:y2]), max(rrpi[x1:x2,y1:y2]), $
      ;           15.5*resfac, 3*resfac, sz[1]/2-x1, sz[2]/2-y1, 7.5*resfac, $
      ;           2.5*resfac ]
      weights = fltarr( sz[1], sz[2] ) + 1.0/stddev(rrpi)^2
      zfit = mpfit_propeller( rrpi[x1:x2,y1:y2], pp1, sigma=pp_sigma, $
                              chisq=chisq, /quiet, estimate=pp_est, /pos, $
                              covar=covar, weights=weights[x1:x2,y1:y2] )
      pp = pp1 + [ 0, 0, 0, 0, x1, y1, 0, 0 ]
      dof = n_elements(zfit)/resfac^2 - n_elements(pp)
    endif else begin
      pp = _pp[*,j]
      pp_sigma = _pp_sigma[*,j]
      chisq = _chisq[j]
      dof = _dof[j]
    endelse
    for k=-1,1,2 do begin
      ppp = pp
      p1 = [ ppp[0:3], ppp[4]+k*ppp[6]+k*ppp[2], ppp[5]-k*ppp[7], 0, 0 ]
      if keyword_set(fwhm) then p1[2:3] = p1[2:3] * sqrt(2*alog(2))
      plots, lindgen((2*p1[2])>1) + p1[4]-p1[2], p1[5] + p1[3]* $
             sqrt( 1 - (( lindgen((2*p1[2])>1) - p1[2] )/p1[2])^2 ), $
             /device, color=ctpurple()
      plots, lindgen((2*p1[2])>1) + p1[4]-p1[2], p1[5] - p1[3]* $
             sqrt( 1 - (( lindgen((2*p1[2])>1) - p1[2] )/p1[2])^2 ), $
             /device, color=ctpurple()
    endfor
    print, '   bg value    peak value       width-X      width-Y     '+$
           'center-X     center-Y   separation-X separation-Y'
    print, pp
    print, pp_sigma * sqrt( chisq / dof )
    ;print, chisq
  endif else stop, 'sgldbl must be 1 or 2.'

  reply = ''
  while reply eq '' do begin
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
           ' / '+datamodeltxt+' / [f]lag with a comment / '+$
           usetxt+' / [r]edo fit'+$
           ' / '+orphantxt+' / [c]ontinue / [q]uit / sa[v]e and quit'
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
        goto, refit
      end
      'p': begin
        datamodel = 3 - datamodel
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
        if plotalso ne -1 then _use[plotalso] = _use[j]
        reply = ''
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
;  save, _x1, _x2, _y1, _y2, _pp, _pp_sigma, _chisq, _dof, _flag, _use, $
;        _orphan, filename='fit_propellers.sav.4'
  while !d.window ne -1 do wdelete, !d.window
  if keyword_set(quit) then begin
    quit = 0
    retall
  endif

endfor

end
