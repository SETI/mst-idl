; If fit_propellers_redge1 has already been run, will measure
; integrated brightness

restore, 'prop_reproj_redge.sav'
npr = n_elements(prop_reproj)
if not keyword_set(j1) then j1 = 0l
if not keyword_set(j2) then j2 = npr - 1

if not keyword_exists(lonsm) then lonsm = 16;30;5
sfile = 'fit_propellers_redge1_'+strtrim(lonsm,2)+'.sav'
restore, sfile

sfile = 'fit_propellers_redge_peak.sav'
if keyword_set(findfile(sfile)) then restore, sfile else begin
  if not keyword_set(nterms) then nterms = 4
  xi = fltarr(npr)
  xo = fltarr(npr)
  yi1 = fltarr(npr)
  yo1 = fltarr(npr)
  yi = fltarr(npr)
  yo = fltarr(npr)
  ai = fltarr(nterms,npr)
  sigma_ai = fltarr(nterms,npr)
endelse

for j=j1,j2 do begin

  rrpi = *( prop_reproj[j].rrpi )
  sz = size(rrpi)
  mnrad = prop_reproj[j].radlon[0]
  mxrad = prop_reproj[j].radlon[1]
  mnlon = prop_reproj[j].radlon[2]
  mxlon = prop_reproj[j].radlon[3]
  radx = make_radi( mnrad, mxrad, mnlon, mxlon, sz, loni=lonx, $
                    dradi=dradx, dloni=dlonx )
  if keyword_set(hipass) then begin
    foo = wher( rrpi eq 0, count )
    sm = 50;10
    rrpi = rrpi - smooth(rrpi,sm)
    if count gt 0 then for k=0,count-1 do begin
      ; Image edge included in rrpi
      rrpi[ (foo[0,k]-sm/2)>0 : (foo[0,k]+sm/2)<(sz[1]-1), $
            (foo[1,k]-sm/2)>0 : (foo[1,k]+sm/2)<(sz[2]-1) ] = 0
    endfor
  endif else sm = 0
  if not keyword_set(xi[j]) then xi[j] = edgebnd[0,j]  ;sm/2
  if not keyword_set(xo[j]) then xo[j] = edgebnd[1,j]  ;sz[1]-sm/2-1
  if not keyword_set(yi1[j]) then yi1[j] = sm/2
  if not keyword_set(yo1[j]) then yo1[j] = sz[2]-sm/2-1
  if not keyword_set(yi[j]) then yi[j] = _guide1[j]
  if not keyword_set(yo[j]) then yo[j] = _guider[j]
  if keyword_set(deshear) then begin
    for x=xi[j],xo[j] do begin
      lon = lonx[x]
      redge = *(prop_redge[j].redge)
;      rj = where( redge[0,*] eq lon, count )
;      if count ne 1 then stop, 'Trouble translating x to redge'
      fitrad = poly( lon-mean(redge[0,a1]), fit1 )
      sh = interpol( findgen(sz[2]), radx-mean(redge[1,a1]), fitrad )
      rrpi[x,*] = shift( rrpi[x,*], sz[2]/2-sh )
    endfor 
  endif 
  rrpi_prof = rebin( rrpi[xi[j]:xo[j],*], 1, sz[2] )

  _ai = [ (max(rrpi_prof[yi[j]:yo[j]])-min(rrpi_prof[yi[j]:yo[j]]))/3., $
          (where(rrpi_prof[yi[j]:yo[j]] eq $
                 max(rrpi_prof[yi[j]:yo[j]])))[0]+yi[j], $
          (yo[j]-yi[j])/3. ]
  if nterms gt 3 then _ai = [ _ai, replicate(0,nterms-3) ]
  if keyword_set(nopad) then begin
    gx = dindgen(yo[j]-yi[j]+1)+yi[j]
    gy = rrpi_prof[yi[j]:yo[j]]
  endif else begin
    gx = dindgen((yo[j]-yi[j])*3+1)+yi[j]*2-yo[j]
    pad = rrpi_prof[yi[j]]<rrpi_prof[yo[j]]
    gy = [ replicate(pad,yo[j]-yi[j]), rrpi_prof[yi[j]:yo[j]], $
           replicate(pad,yo[j]-yi[j]) ]
  endelse
  gi = gaussfit( radx[gx], gy, _ai, nterms=nterms, sigma=_sigma_ai )
  if not keyword_set(nopad) then gi = gi[yo[j]-yi[j]:2*(yo[j]-yi[j])]
  ai[*,j] = _ai
  sigma_ai[*,j] = _sigma_ai
  print, ai[*,j]
  print, sigma_ai[*,j]

; EW is rrpi_prof*(yo1[j]-yi1[j])*dradx
  plot_nosci, radx[yi1[j]:yo1[j]]-radx[sz[2]/2], $
              rrpi_prof[yi1[j]:yo1[j]]*(yo1[j]-yi1[j])*dlonx, $
              yr=[ min(rrpi_prof[yi[j]:yo[j]])*(yo1[j]-yi1[j])*dlonx, $
                   max(rrpi_prof[yi[j]:yo[j]])*(yo1[j]-yi1[j])*dlonx ], $
              xtit='Radius Relative to Local Mean (km)', $
              ytit='Equivalent Width (km)', /xs
  plot_nosci, radx[yi1[j]:yo1[j]], rrpi_prof[yi1[j]:yo1[j]], /xs, $
             yr=[ min(rrpi_prof[yi[j]:yo[j]]), max(rrpi_prof[yi[j]:yo[j]]) ], $
             xtit='Radius (km)', ytit='Integrated Brightness (I/F)'
  oplot, radx[yi[[j,j]]], !y.crange, l=1
  oplot, radx[yo[[j,j]]], !y.crange, l=1
  radx10 = make_radi( radx[yi[j]], radx[yo[j]], 0, 0, [0,0,(yo[j]-yi[j])*10] )
  gaussian1, radx10, ai[*,j], g10
  oplot, radx10, g10, co=ctgreen()

  stop

endfor

stop, 'Save?'
save, xi, xo, yi1, yo1, yi, yo, ai, sigma_ai, filename=sfile

end
