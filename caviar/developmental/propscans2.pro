if not keyword_exists(test) then test = 1
print, 'You should have already opened in Caviar an image showing the propeller, and you should have already used @kidz to indicate the radial region to scan.'

if keyword_set(test) then begin
  if test eq 2 then begin
    image_name = 'N1870702753_1_cal.IMG'
    stop, 'Run @caviar and then .c'
    _mnlon = 5.66253
    _mxlon = 5.66553
  endif
  if keyword_set(test) then mnrad = 134899.0d0
  if keyword_set(test) then mxrad = 134925.0d0
  if keyword_set(test) then lonwidth = 0.003d0
endif

if not keyword_set(radarray) then radarray = 0
if n_elements(radarray) ne n_elements(rawim) then begin
  print, 'Getting radarray...'
  get_radarray, cam_params, cmat, nl, et, polera, poledec, sc, $
                radarray, lonarray, /nointerp, planet_coords=planet_coords
endif
radarray = radarray - radscan_radial_offset[jjj]

if keyword_set(_mnlon) then mxlon = _mnlon else begin
  mxlon = keywords.ringplane_least_orbital_longitude
endelse
count = 1
started = 0
while count ne 0 do begin
  mnlon = mxlon
  mxlon = mnlon + lonwidth
  foo = where( radarray gt mnrad and radarray lt mxrad and $
               lonarray gt mnlon and lonarray lt mxlon, count )
  if count gt 0 then begin
    started = 1
    foo = foo[sort(radarray[foo])]
    meanrad = mean([mnrad,mxrad])
    ;foo_bg = where( ( ( radarray gt mnrad and radarray lt meanrad-3 ) or $
    ;                  ( radarray gt meanrad+3 and radarray lt mxrad ) ) and $
    ;              lonarray gt mnlon and lonarray lt mxlon, count )
    ;foo_bg = foo_bg[sort(radarray[foo_bg])]
    radius = radarray[foo]
    prof = rawim[foo]
    level = median(prof)
    ;level = median(rawim[foo_bg])
    profsm = smooth( prof, 20 )
;    foo = 
;    mom1 = 
    plot, tkm(radius), prof, /ynoz, $
          xtit='Radius'+tkmtit(), ytit='I/F', /xs, /ys, yr=[.015,.04], $
          tit='['+strtrim(mnlon,2)+','+strtrim(mxlon,2)+']'
    oplot, !x.crange, [level,level], l=1
    wait, 0.5
    if keyword_set(_mxlon) then begin
      if mxlon ge _mxlon then count = 0
    endif 
  endif
  if not keyword_set(started) then count = 1
endwhile

end
