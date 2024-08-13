; This code is based on $DATA/images/SOI/SOISPTURN/make_sgaps_redge.pro

notn = replicate(' ',20)
;if not keyword_set(prop_reproj) then restore, 'prop_reproj.sav'
if not keyword_set(prop_reproj) then restore, 'prop_reproj_redge.sav'
npr = n_elements(prop_reproj)
if not keyword_set(j1) then j1 = 0l
if not keyword_set(j2) then j2 = npr - 1
if not keyword_set(origres) then rfac1 = 3. ; display only
if rfac1 mod 2 eq 0 then print, 'It''s best for rfac1 to be odd.'
if rfac1 eq 1 then offs=0 else offs=rfac1/2-0.1

if not keyword_exists(lonsm) then lonsm = 16;30;5
sfilestem = 'fit_propellers_redge1'
sfile = sfilestem+'_'+strtrim(lonsm,2)+'.sav'
if keyword_set(impact) then if impact eq 2 then sfile = sfilestem+'.sav'
if keyword_set(findfile(sfile)) then restore, sfile else stop, 'No file'
if not keyword_set(subtractavg) then subtractavg = bytarr(npr)
if not keyword_set(hipass) then hipass = bytarr(npr)
if n_elements(hipass) eq 1 then hipass = rebin( hipass, npr )

; Factors for calculating mass (see $DATA/images/SOI/SOISPTURN/sgaps_mass.pro)
cap_g = 6.672e-8   ;cm^3/g/s^2
ms = 5.685e29  ;g
rho = .5e15    ;g/km^3 

for j=j1,j2 do begin

  ; Unpack reprojected image and associated value, define grid arrays.
  rrpi = *( prop_reproj[j].rrpi )
  sz = size(rrpi)
  mnrad = prop_reproj[j].radlon[0]
  mxrad = prop_reproj[j].radlon[1]
  mnlon = prop_reproj[j].radlon[2]
  mxlon = prop_reproj[j].radlon[3]
  radx = make_radi( mnrad, mxrad, mnlon, mxlon, sz, loni=lonx, $
                    dradi=dradx, dloni=dlonx )
  if keyword_set(hipass[j]) then begin
    foo = wher( rrpi eq 0, count )
    sm = 50
    rrpi = rrpi - smooth(rrpi,sm)
    if count gt 0 then for k=0,count-1 do begin
      ; Image edge included in rrpi
      rrpi[ (foo[0,k]-sm/2)>0 : (foo[0,k]+sm/2)<(sz[1]-1), $
            (foo[1,k]-sm/2)>0 : (foo[1,k]+sm/2)<(sz[2]-1) ] = 0
    endfor
  endif
  if keyword_set(subtractavg[j]) then begin
    ; If the edge of the image seems to be included, then set contaminated
    ; values equal to image median to avoid confusing the fitting routine.
    foo = wher( rrpi eq 0, count )
    mask = bytarr( sz[1], sz[2] ) + 1
    if count gt 10 then begin
      print, 'rrpi contains zeroes, so assume it goes off the edge of the image.'
      box = 1;5
      if prop_reproj[j].xy[2] gt 850 then box = 0
      for k=0l,count-1 do begin
        mask[ (foo[0,k]-box)>0 : (foo[0,k]+box)<(sz[1]-1), $
              (foo[1,k]-box)>0 : (foo[1,k]+box)<(sz[2]-1) ] = 0
      endfor
      rrpi[where( mask eq 0 )] = median(rrpi[where( mask eq 1 )])
    endif
    rrpi = fit_propellers4_subtractavg( rrpi, rrpi, mask, sz, edgebnd=edgebnd[*,j] )
    rrpi = rrpi + (1-mask)*1e-10  ; Keep edgefit from drawing erroneous bke's
  endif

  foo = where( rrpi eq 0, count )
  if count gt 0 then rrpi[foo] = median(rrpi[where(rrpi ne 0)])
  rrpi_orig = rrpi

  hthresh = 0;5
  run_histogram, rrpi, stmin, stmax, threshold=hthresh, /nocrop, /silent
  rpi = (rrpi_orig>stmin<stmax) - stmin
  rpi = rpi*248 / max(rpi)
  rpi = byte(rpi)
  rpi = rebin( rpi, sz[1]*rfac1, sz[2]*rfac1, /sample )
  window, 10, xs=sz[1]*rfac1+10, ys=sz[2]*rfac1
  tv, rpi

  for k=-rfac1/2,rfac1/2 do begin
    plots, /device, interpol(indgen(sz[1]),lonx,redge[0,foo])*rfac1+offs+k, $
           redge[3,foo]*rfac1+offs, ps=3, color=ctgreen()
  endfor
;  wdelete, 8

  redo2:
  window, 9
  !y.margin = 0
  !y.omargin = [4,2]
  !p.multi = [0,1,2]
  if keyword_set(impact) then begin
    good0 = where( redge[0,*] lt lonx[sz[1]-rem-1], ng0 )
  endif else begin
    good0 = where(( redge[0,*] gt lonx[edgebnd[0,j]] and $
                    redge[0,*] le lonx[edgebnd[1,j]] ) or $
                  ( redge[0,*] ge lonx[edgebnd[2,j]] and $
                    redge[0,*] lt lonx[edgebnd[3,j]] ),ng0)
  endelse
  yr1 = [ min(redge[1,good0]), max(redge[1,good0]) ]
  yr1 = yr1 + [-0.1,0.1]*(yr1[1]-yr1[0])
  plot_nosci, redge[ 0, good0[[0,ng0-1]] ], yr1, /nodata, /xs, /ys, $
              ytit='Radius (km)', xtickn=notn
  polyfill, [ reform(redge[0,good0]), reverse(reform(redge[0,good0])) ], $
            [ reform(redge[1,good0]-redge_sigma[1,good0]), $
              reverse(reform(redge[1,good0]+redge_sigma[1,good0])) ], $
            noclip=0, color=ctgray()
  oplot, redge[0,good0], redge[1,good0]
  if keyword_set(impact) and keyword_set(prop_radx[*,j]) then begin
    oplot, !x.crange, prop_radx[[0,0],j], color=ctcyan()
  endif 
  yr2 = [ min(redge[2,good0]), max(redge[2,good0]) ]
  yr2 = yr2 + [-0.1,0.1]*(yr2[1]-yr2[0])
  plot_nosci, redge[ 0, good0[[0,ng0-1]] ], yr2, /nodata, /xs, /ys, $
              ytit='Peak Depth (I/F)',xtit='Longitude (!Uo!N)'
  polyfill, [ reform(redge[0,good0]), reverse(reform(redge[0,good0])) ], $
            [ reform(redge[2,good0]-redge_sigma[2,good0]), $
              reverse(reform(redge[2,good0]+redge_sigma[2,good0])) ], $
            noclip=0, color=ctgray()
  oplot, redge[0,good0], redge[2,good0]

  if keyword_set(edgebnd[1,j]) and not keyword_set(redo) then begin
    for k=0,nedgebnd-1 do begin
      wset, 9
      oplot, ledgebnd[[k,k],j], color=ctblue(), $
             !y.crange[0] + (!y.crange[1]-!y.crange[0])*[0,2], /noclip
      wset, 10
      if keyword_set(impact) then offs2 = [0,rfac1-1] else begin
        offs2 = [0,rfac1-1,0,rfac1-1]
      endelse 
      plots, edgebnd[[k,k],j]*rfac1+offs2[k], $
             [0,sz[2]]*rfac1, /device, color=ctblue()
    endfor 
    bnd = edgebnd[*,j]
  endif else begin
    bnd = intarr(nedgebnd)
    print, 'Click to the right and left of the propeller.'
    if keyword_set(impact) then begin
      offs1 = [ 0, -1 ]
    endif else begin
      offs1 = [ rem1, rem1+1, cenbnd[1,j]-1, cenbnd[1,j] ]
    endelse
    for k=0,nedgebnd-1 do begin
      wset, 9
      ;cursor, bnd1, y, 3, /device
      ;bnd[k] = bnd1/lonsm / rfac1
      cursor, bnd1, y, 3, /data
      bnd[k] = ( interpol(indgen(sz[1]),lonx,bnd1) - offs1[k] ) / lonsm
      bnd[k] = bnd[k] * lonsm + offs1[k]
      oplot, lonx[bnd[[k,k]]], color=ctblue(), $
             !y.crange[0] + (!y.crange[1]-!y.crange[0])*[0,2], /noclip
      wset, 10
      plots, bnd[[k,k]]*rfac1+([0,rfac1-1,0,rfac1-1])[k], [0,sz[2]]*rfac1, $
             /device, color=ctblue()
    endfor
    edgebnd[*,j] = bnd
    ledgebnd[*,j] = lonx[bnd]
  endelse

  mnl = mean(lonx)
  good1 = where(( redge[0,*] gt lonx[edgebnd[0,j]] and $
                  redge[0,*] lt lonx[edgebnd[1,j]] ),ng1)
  a1 = indgen(( edgebnd[1,j] - edgebnd[0,j] - 1 )/lonsm)*lonsm + $
       edgebnd[0,j] + 1 + lonsm/2
  for k=0,n_elements(a1)-1 do a1[k] = (where( $
     abs(redge[0,*]-lonx[a1[k]]) eq min(abs(redge[0,*]-lonx[a1[k]])) ))[0]
  if keyword_set(impact) then begin
    goodlonx = good1
    if impact eq 2 then begin
      fit1[*,j] = [0,0]
      sigma1[*,j] = [0,0]
      prop_radx[*,j] = mean(redge[1,a1])
      prop_radx_sigma[*,j] = stddev(redge[1,a1]) / sqrt(float(ng1)/lonsm)
      cenbnd[*,j] = mean(edgebnd[*,j])
      lcenbnd[*,j] = mean(ledgebnd[*,j])
    endif else begin
      fit1[*,j] = svdfit( reform(redge[0,a1]) - mean(redge[0,a1]), $
                          reform(redge[1,a1]) - mean(redge[1,a1]), $
                          2, sigma=_sigma1 )
      sigma1[*,j] = _sigma1
      cenbnd[*,j] = mean(edgebnd[*,j])
      diff = 0
      while 4 lt 5 do begin
        ldiff = diff
        diff = total(redge[2,cenbnd[*,j]+1:edgebnd[1,j]]) - $
               total(redge[2,edgebnd[0,j]:cenbnd[*,j]])
        if sign(diff) eq sign(ldiff) then begin
          cenbnd[*,j] = cenbnd[*,j] + sign(diff)
        endif else begin
          if abs(diff) gt abs(ldiff) then cenbnd[*,j] = cenbnd[*,j] + sign(diff)
          goto, cenbnd_done
        endelse 
      endwhile 
      cenbnd_done:
      lcenbnd[*,j] = mean([ lonx[cenbnd[*,j]], lonx[cenbnd[*,j]+1] ])
      wset, 9
      oplot, lcenbnd[[0,0],j], color=ctcyan(), $
             !y.crange[0] + (!y.crange[1]-!y.crange[0])*[0,2], /noclip
      prop_radx[*,j] = poly(lcenbnd[*,j]-mean(redge[0,a1]),fit1[*,j]) + $
                       mean(redge[1,a1])
      lcenbnd_sigma[*,j] = lonsm * dlonx
      prop_radx_sigma[*,j] = fit1[1,j] * lcenbnd_sigma[*,j]
      ; Kepler shear:  da/dn = -(2/3)*a/n
      nn = sqrt(caviar_omega2( mean(redge[1,a1]) ))*180/!dpi ; deg/sec
      age[j] = -2./3 * prop_radx[*,j] / nn / 3600 / fit1[1,j]
      sigma_age[j] = 2./3 * prop_radx[*,j] / nn / 3600 / fit1[1,j]^2 * $
                     sigma1[1,j]
      print, 'Estimated time elapsed since impact:  '+strtrim(age[j],2)+' hours'
    endelse
    wset, 9
    axis, yr=yr1-(yr1[1]-yr1[0]), yaxis=0, /ys, /save, ytickn=notn, $
          ytickle=1e-10
    oplot, ledgebnd[*,j], co=ctyellow(), /noclip, $
           poly(ledgebnd[*,j]-mean(redge[0,a1]),fit1[*,j]) + mean(redge[1,a1])
  endif else begin
    good2 = where(( redge[0,*] gt lonx[edgebnd[2,j]] and $
                    redge[0,*] lt lonx[edgebnd[3,j]] ))
    goodlonx = [ good1, good2 ]
    a2 = indgen(( edgebnd[3,j] - edgebnd[2,j] - 1 )/lonsm)*lonsm + $
         edgebnd[2,j] + 1 + lonsm/2
    for k=0,n_elements(a2)-1 do a2[k] = (where( $
       abs(redge[0,*]-lonx[a2[k]]) eq min(abs(redge[0,*]-lonx[a2[k]])) ))[0]
;    mn1 = svdfit( reform(redge[0,a1]), reform(redge[1,a1]), 1, sigma=_sigma1, $
;                  measure_errors=reform(redge_sigma[1,a1]) )
;    mn2 = svdfit( reform(redge[0,a2]), reform(redge[1,a2]), 1, sigma=_sigma2, $
;                  measure_errors=reform(redge_sigma[1,a2]) )
    mn1 = svdfit( reform(redge[0,a1]), reform(redge[1,a1]), 1, sigma=_sigma1 )
    mn2 = svdfit( reform(redge[0,a2]), reform(redge[1,a2]), 1, sigma=_sigma2 )
    prop_radx[*,j] = [ mn1, mn2 ]
    prop_radx_sigma[*,j] = [ _sigma1, _sigma2 ]
    wset, 9
    axis, yr=yr1-(yr1[1]-yr1[0]), yaxis=0, /ys, /save, ytickn=notn, $
          ytickle=1e-10
    for k=0,nedgebnd/2-1 do begin
      oplot, ledgebnd[[k*2,k*2+1],j], prop_radx[[k,k],j], co=ctyellow(), /noclip
      oplot, ledgebnd[[k*2,k*2+1],j], /noclip, $
             prop_radx[[k,k],j] - prop_radx_sigma[[k,k],j], co=ctyellow()
      oplot, ledgebnd[[k*2,k*2+1],j], /noclip, $
             prop_radx[[k,k],j] + prop_radx_sigma[[k,k],j], co=ctyellow()
    endfor 

    dr = prop_radx[0,j] - prop_radx[1,j]
    dr_sigma = sqrt(total(prop_radx_sigma[*,j]^2))
    aa = mean(prop_radx[*,j])
    hh = dr / 4
    h_sigma = dr_sigma / 4
    hfac1 = aa * (4*!dpi*rho/9/ms)^(1./3)
    rr = hh / hfac1
    r_sigma = h_sigma / hfac1
    print, 'Propeller diameter:  '+string(rr*2000,fo='(I4)')+' +- '+$
           string(r_sigma*2000,fo='(I3)')+' meters'
  endelse

  reply = ''
  while reply eq '' do begin
    print, 'Redo step [1], redo step [2], or [c]ontinue or [q]uit?'
    read, reply
    case reply of
      '1': begin
        wset, 10
        tv, rpi
        if not keyword_set(impact) then begin
          for k=0,1 do begin
            plots, cenbnd[[k,k],j]*rfac1+([rfac1-1,0])[k], $
                   [0,sz[2]]*rfac1, /device, color=ctltcyan()
          endfor 
          cenbnd[*,j] = 0
          lcenbnd[*,j] = 0
          rem1 = 0
        endif 
        plots, /device, [0,sz[1]]*rfac1, [1,1]*_guide1[j]*rfac1, color=ctltred()
        plots, /device, [0,sz[1]]*rfac1, [1,1]*_guider[j]*rfac1+rfac1-1, $
               color=ctltred()
        _guide1[j] = 0
        _guider[j] = 0
        edgebnd[*,j] = 0
        ledgebnd[*,j] = 0
;        goto, redo1
      end 
      '2': begin
        wset, 10
        tv, rpi
        if not keyword_set(impact) then begin
          for k=0,1 do begin
            plots, cenbnd[[k,k],j]*rfac1+([rfac1-1,0])[k], $
                   [0,sz[2]]*rfac1, /device, color=ctcyan()
          endfor 
          cenbnd[*,j] = 0
          lcenbnd[*,j] = 0
          rem1 = 0
        endif 
        plots, /device, [0,sz[1]]*rfac1, [1,1]*_guide1[j]*rfac1, color=ctred()
        plots, /device, [0,sz[1]]*rfac1, [1,1]*_guider[j]*rfac1+rfac1-1, $
               color=ctred()
        for k=-rfac1/2,rfac1/2 do begin
          plots, /device, interpol(indgen(sz[1]),lonx,redge[0,foo])*rfac1+$
                 offs+k, redge[3,foo]*rfac1+offs, ps=3, color=ctgreen()
        endfor
        edgebnd[*,j] = 0
        ledgebnd[*,j] = 0
        goto, redo2
      end 
      'q': retall
      'c': begin
      end 
      else: reply = ''
    endcase 
  endwhile 

  save, _guide1, _guider, edgebnd, ledgebnd, nedgebnd, cenbnd, lcenbnd, lonsm, $
        prop_radx, prop_radx_sigma, prop_redge, subtractavg, hipass, sm, _lonsm, $
        age, sigma_age, fit1, sigma1, impact, lcenbnd_sigma, filename=sfile

endfor

end
