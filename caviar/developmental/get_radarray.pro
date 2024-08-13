pro get_radarray, cam_params, cmat, nl, et, polera, poledec, sc, radarray, lonarray, outofplane=outofplane, nointerp=nointerp, minmaxonly=minmaxonly, planet_coords=planet_coords

if keyword_set(minmaxonly) then begin
  xarray = [ lindgen(nl), replicate(nl-1,nl), lindgen(nl), replicate(0l,nl) ]
  yarray = [ replicate(nl-1,nl), lindgen(nl), replicate(0l,nl), lindgen(nl) ]
endif else begin
  if keyword_set(nointerp) then int = 1 else int = 32
  xarray = rebin( lindgen(nl/int+1)*int, nl/int+1, nl/int+1 )
  yarray = rebin( lindgen(1,nl/int+1)*int, nl/int+1, nl/int+1 )
endelse
p2radec_quicker, cam_params, cmat, nl, yarray, xarray, ra, dec
p2ralon_quicker, cmat, et, polera, poledec, sc, RA, dec, rad, lon, $
                 outofplane=outofplane
if keyword_set(outofplane) then return

; Find the longitude interval contained in the image, 
; even if it wraps through 0/360
lthres = 90
ll = lon[sort(lon)]
dll = ll[1:n_elements(ll)-1] - ll[0:n_elements(ll)-2]
if max(dll) gt lthres then begin
  _lmin = (ll[where( dll eq max(dll) )+1])[0]
  lon[where(lon ge _lmin)] = lon[where(lon ge _lmin)] - 360
endif

if not keyword_set(nointerp) and not keyword_set(minmaxonly) then begin
  if max(lon) gt 350 and min(lon) lt 10 then begin
    print, 'Get_Radarray:  Image covers more than '+strtrim(360-lthres,2)+' degrees of longitude range.'
    ;if keyword_set(minmaxonly) then begin
    ;  print, 'Proceed anyway, since we''re only interested in max and min.'
    ;  minmaxonly = 2
    ;endif else begin
      print, 'You should use a routine that does not interpolate.  Proceed?'
      stop
    ;endelse
  endif
endif

if keyword_set(minmaxonly) then begin
  radarray = rad
  lonarray = lon
endif else begin
  radarray = interpolate( rad, findgen(nl)/int, findgen(nl)/int, /grid )
  lonarray = interpolate( lon, findgen(nl)/int, findgen(nl)/int, /grid )
endelse

if keyword_set(planet_coords) then begin
  if (where( planet_coords[0,*]>0<(nl-1) ne planet_coords[0,*]))[0] eq -1 then begin
    print, 'Saturn is contained within the image:  '+$
           strtrim(planet_coords[0,0],2)+'  '+strtrim(planet_coords[0,1],2)
    if keyword_set(minmaxonly) then begin
      lonarray[0:1] = [0,360]
      if radarray[0] ne max(rad) then radarray[0]=0 else radarray[1]=0
    endif
  endif
endif
;if keyword_set(minmaxonly) then if minmaxonly eq 2 then lonarray[0:1] = [0,360]

if not keyword_set(minmaxonly) then begin
  foo = where( lonarray lt 0, count )
  if count gt 0 then lonarray[foo] = lonarray[foo] + 360
endif

end
