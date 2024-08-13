if not keyword_set(prop_reproj) then restore, 'prop_reproj.sav'
npr = n_elements(prop_reproj)
if not keyword_set(j1) then j1 = 0l
if not keyword_set(j2) then j2 = npr - 1
if keyword_set(findfile('fit_propellers.sav')) then begin
  restore, 'fit_propellers.sav'
endif else begin
  _x1 = lonarr(npr)
  _x2 = lonarr(npr)
  _y1 = lonarr(npr)
  _y2 = lonarr(npr)
  _lonsm = replicate( 5, npr )
  _radsm = lonarr(npr)
  _lonpp = dblarr( 4, npr )
  _radpp = dblarr( 4, npr )
  _lonpp_sigma = dblarr( 4, npr )
  _radpp_sigma = dblarr( 4, npr )
  _lonchisq = dblarr(npr)
  _radchisq = dblarr(npr)
endelse
if not keyword_exists(auto) then auto = 1
if not keyword_set(auto) then auto = 0

for j=j1,j2 do begin
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
  _lonx = lonx & _radx = radx
  window, 12, xs=sz[1], ys=sz[2]
  run_histogram, rrpi, stmin, stmax, threshold=5, /nocrop, /silent
  tvscl, rrpi>stmin<stmax
  if not keyword_set(_x1[j]) then begin
    domanual:
    print, 'Click on lower-left corner of area to select.'
    cursor, x1, y1, 3, /device
    _x1[j] = x1 & _y1[j] = y1
    print, 'Click on upper-right corner of area to select.'
    cursor, x2, y2, 3, /device
    _x2[j] = x2 & _y2[j] = y2
    save, _x1, _x2, _y1, _y2, _lonsm, _radsm, _lonpp, _radpp, $
          _lonpp_sigma, _radpp_sigma, _lonchisq, _radchisq, $
          filename='fit_propellers.sav'
  endif else begin
    x1 = _x1[j] & x2 = _x2[j] & y1 = _y1[j] & y2 = _y2[j]
  endelse
  redo:
  plots, [ x1, x2, x2, x1, x1 ], [ y1, y1, y2, y2, y1 ], color=ctblue(), /dev
  reply = ''
  if not keyword_set(auto) then while reply eq '' do begin
    print, 'Continue? (c/n/q)'
    read, reply
    if reply eq 'q' then retall else if reply eq 'n' then goto, domanual $
                                else if reply ne 'c' then reply = ''
  endwhile
  mask = bytarr( sz[1], sz[2] ) + 1
  foo = wher( rrpi eq 0, count )
  if count gt 10 then begin
    print, 'rrpi contains zeroes, so assume it goes off the edge of the image.'
    box = 5
    for k=0,count-1 do begin
      mask[ (foo[0,k]-box)>0 : (foo[0,k]+box)<(sz[1]-1), $
            (foo[1,k]-box)>0 : (foo[1,k]+box)<(sz[2]-1) ] = 0
    endfor
  endif
  if keyword_set(_lonsm[j]) then lonsm = _lonsm[j] else lonsm = 0
  if keyword_set(_radsm[j]) then radsm = _radsm[j] else radsm = 0
  if 4 eq 5 then begin
    dorsmooth:
    print, 'Enter new y-smoothing length (or ''q'' to quit):'
    read, reply
    if reply eq 'q' then retall else radsm = reply
    dolsmooth:
    print, 'Enter new x-smoothing length (or ''q'' to quit):'
    read, reply
    if reply eq 'q' then retall else lonsm = reply
  endif
  lonprof = dblarr(sz[1]) & lonsem = dblarr(sz[1])
  radprof = dblarr(sz[2]) & radsem = dblarr(sz[2])
  for k=0,sz[1]-1 do begin
    rrpiint = rrpi[(k-lonsm)>0:(k+lonsm)<(sz[1]-1),y1:y2]
    maskint = mask[(k-lonsm)>0:(k+lonsm)<(sz[1]-1),y1:y2]
    ;lonprof[k] = mean(rrpi[(k-lonsm)>0:(k+lonsm)<(sz[1]-1),y1:y2])
    lonprof[k] = total( (rrpiint*maskint) ) / total( maskint )
    ;lonsem[k] = stddev(rrpi[(k-lonsm)>0:(k+lonsm)<(sz[1]-1),y1:y2]) / $
    ;            sqrt(y2-y1+1)
    lonsem[k] = sqrt( total( (rrpiint - lonprof[k])^2 * maskint ) / $
                      product( total( maskint )+[0,-1] ) )
  endfor
  for k=0,sz[2]-1 do begin
    rrpiint = rrpi[x1:x2,(k-radsm)>0:(k+radsm)<(sz[1]-1)]
    maskint = mask[x1:x2,(k-radsm)>0:(k+radsm)<(sz[1]-1)]
    ;radprof[k] = mean(rrpi[x1:x2,(k-radsm)>0:(k+radsm)<(sz[2]-1)])
    radprof[k] = total( (rrpiint*maskint) ) / total( maskint )
    ;radsem[k] = stddev(rrpi[x1:x2,(k-radsm)>0:(k+radsm)<(sz[2]-1)]) / $
    ;            sqrt(x2-x1+1)
    radsem[k] = sqrt( total( (rrpiint - radprof[k])^2 * maskint ) / $
                      product( total( maskint )+[0,-1] ) )
  endfor
  good = where( finite(lonprof), count )
  if count eq 0 then stop, 'No good points in lonprof'
  if count lt sz[1] then begin
    lonprof = lonprof[good]
    lonsem = lonsem[good]
    lonx = _lonx[good]
    sz[1] = count
  endif
  good = where( finite(radprof), count )
  if count eq 0 then stop, 'No good points in radprof'
  if count lt sz[2] then begin
    radprof = radprof[good]
    radsem = radsem[good]
    radx = _radx[good]
    sz[2] = count
  endif
  lonweights = replicate( 1.0d0, sz[1] )
  radweights = replicate( 1.0d0, sz[1] )
  lonpp = [ mean(lonprof), mean([x1,x2]), (x2-x1)/2, 0 ]
  radpp = [ mean(radprof), mean([y1,y2]), (y2-y1)/2, 0 ]
  lonmodel = mpcurvefit( findgen(sz[1]), lonprof, lonweights, lonpp, $
                         lonpp_sigma, chisq=lonchisq, $
                         function_name='gaussian1', /quiet )
  radmodel = mpcurvefit( findgen(sz[2]), radprof, radweights, radpp, $
                         radpp_sigma, chisq=radchisq, $
                         function_name='gaussian1', /quiet )
  lonhm = mean([ min(lonmodel[[0,sz[1]-1]]), max(lonmodel) ])
  lonbe = [ interpol( (findgen(sz[1]))[0:lonpp[1]], $
                      lonmodel[0:lonpp[1]], lonhm ), $
            interpol( (findgen(sz[1]))[lonpp[1]:sz[1]-1], $
                      lonmodel[lonpp[1]:sz[1]-1], lonhm ) ]
  radhm = mean([ min(radmodel[[0,sz[2]-1]]), max(radmodel) ])
  radbe = [ interpol( (findgen(sz[2]))[0:radpp[1]], $
                      radmodel[0:radpp[1]], radhm ), $
            interpol( (findgen(sz[2]))[radpp[1]:sz[2]-1], $
                      radmodel[radpp[1]:sz[2]-1], radhm ) ]
  window, 13
  !p.multi = [0,1,2]
  plot_nosci, /xs, /ys, lonx, lonprof, xtit='Longitude (!Uo!N)', ytit='I/F'
  polyfill, noclip=0, color=ctgray(), [ lonx, reverse(lonx) ], $
            [ lonprof-lonsem, reverse(lonprof+lonsem) ]
  oplot, lonx, lonprof
  oplot, lonx, lonmodel, co=ctgreen()
  oplot, lonx[ lonbe[[0,0]] ], !y.crange, co=ctpurple()
  oplot, lonx[ lonbe[[1,1]] ], !y.crange, co=ctpurple()
  plot_nosci, /xs, /ys, radx, radprof, xtit='Radius (km)', ytit='I/F'
  polyfill, noclip=0, color=ctgray(), [ radx, reverse(radx) ], $
            [ radprof-radsem, reverse(radprof+radsem) ]
  oplot, radx, radprof
  oplot, radx, radmodel, co=ctgreen()
  oplot, radx[ radbe[[0,0]] ], !y.crange, co=ctpurple()
  oplot, radx[ radbe[[1,1]] ], !y.crange, co=ctpurple()
  wset, 12
  tvscl, rrpi>stmin<stmax
  plots, [ x1, x2, x2, x1, x1 ], [ y1, y1, y2, y2, y1 ], color=ctblue(), /dev
;  plots, /dev, lonbe[[0,1,1,0,0]], radbe[[0,0,1,1,0]], color=ctpurple()
  londiff = round(lonbe[1]) - round(lonbe[0])
  plots, lindgen(londiff) + round(lonbe[0]), $
         mean(round(radbe)) + (round(radbe[1])-round(radbe[0]))/2* $
         sqrt( 1 - (( lindgen(londiff) - londiff/2.0d0 )/(londiff/2))^2 ), $
         /device, color=ctpurple()
  plots, lindgen(londiff) + round(lonbe[0]), $
         mean(round(radbe)) - (round(radbe[1])-round(radbe[0]))/2* $
         sqrt( 1 - (( lindgen(londiff) - londiff/2.0d0 )/(londiff/2))^2 ), $
         /device, color=ctpurple()
  reply = ''
  if auto le 1 then while reply eq '' do begin
    print, 'Longitudinal smoothing length = '+strtrim(lonsm,2)
    print, 'Radial smoothing length = '+strtrim(radsm,2)
    print, 'Continue or Adjust? (c/l/r/q)'
    read, reply
    if reply eq 'q' then retall else if reply eq 'r' then goto, dorsmooth $
                                else if reply eq 'l' then goto, dolsmooth $
                                else if reply ne 'c' then reply = ''
  endwhile
  sz = size(rrpi)
  foo = where( abs( [x1,x2,y1,y2] - round([lonbe,radbe])>0<(sz[[1,2,1,2]]-1) ) gt 1, count )
  if count gt 1 then begin
    x1 = round(lonbe[0])>0 & x2 = round(lonbe[1])<(sz[1]-1)
    y1 = round(radbe[0])>0 & y2 = round(radbe[1])<(sz[2]-1)
    goto, redo
  endif
  _lonsm[j] = lonsm & _radsm[j] = radsm
  _lonpp[*,j] = lonpp & _radpp[*,j] = radpp
  _lonpp_sigma[*,j] = lonpp_sigma & _radpp_sigma[*,j] = radpp_sigma
  _lonchisq[j] = lonchisq & _radchisq[j] = radchisq
  save, _x1, _x2, _y1, _y2, _lonsm, _radsm, _lonpp, _radpp, $
        _lonpp_sigma, _radpp_sigma, _lonchisq, _radchisq, $
        filename='fit_propellers.sav'
endfor

end
