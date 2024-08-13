print, 'IMPORTANT NOTE:  This module may not work properly if there are any defined variables.  Either run this module in a new IDL session or type ".reset"'

if not keyword_exists(nocheck) then nocheck = 1
if not keyword_set(findfile('stretch.sav')) then begin

  reply = ''
  while reply eq '' do begin
    print, 'Search for only images with "_cal"? (y/n)'
    read, reply
    if reply eq 'y' then cal='_cal' else if reply eq 'n' then cal='' else reply=''
  endwhile

  new = 1
  ff = findfile('*'+cal+'.IMG')
  nadd = n_elements(ff)
  add = indgen(nadd)

endif else begin

  restore, 'stretch.sav'
  if keyword_set(findfile('et.sav')) then restore, 'et.sav'

  new = 0
  cal = strpos(filenames[0],'cal')
  if cal eq -1 then cal = '' else cal = '_cal'
  ff = findfile('*'+cal+'.IMG')
  nff = n_elements(ff)
  if nff lt n_elements(filenames) then stop, 'Files listed in filenames not found.'
  if nff gt n_elements(filenames) then begin
    proceed = 1
    add = 0
    for j=0,nff-1 do if (where(ff[j] eq filenames))[0] eq -1 then add=[add,j]
    if nff-n_elements(add)+1 ne n_elements(filenames) then stop, $
     	   			   'Files listed in filenames not found.'
    add = clip(add)
    nadd = n_elements(add)
  endif

endelse

if not keyword_exists(usehistogram) then usehistogram = 1
if keyword_set(usehistogram) then set='' else set=' not'
print, 'UseHistogram'+set+' set.  Change this if you want to.'
if not keyword_exists(basiconly) then basiconly = 1
if keyword_set(diffuse) then set='' else set=' not'
print, 'For marking ring radii, this sequence is'+set+' of Diffuse Rings.  Change this if you want to.'
if keyword_set(new) or keyword_set(proceed) then begin
  for jjj=0,nadd-1 do begin
    image_name = ff[add[jjj]]
    print, '------'
    print, strtrim(jjj,2)+' / '+strtrim(nadd-1,2)+'   '+image_name
    print, '------'
    cal = strpos( image_name, '_cal' )
    if cal eq -1 then noncalfn = image_name else noncalfn = strmid(image_name,0,cal)+strmid(image_name,cal+4,strlen(image_name))
    if not keyword_set(nobl) then mark_badlines, noncalfn, nocheck=nocheck
    ;cspice_clpool
    noplot = 1
    @caviar
    minmaxonly = 1
    get_radarray, cam_params, cmat, nl, et, polera, poledec, sc, radarray, lonarray, outofplane=outofplane, minmaxonly=minmaxonly, planet_coords=planet_coords
    if keyword_set(do_radial_scale_adj) then begin
      nazvec = 1
      @calculate_keywords2a
      if abs( radial_scale - $
              keywords.ringplane_aimpoint_radial_scale ) gt .001 then stop
    endif 
    if keyword_set(outofplane) then begin
      _rmin = 60330. & _rmax = 1e6
      _lmin = 0. & _lmax = 360.
    endif else begin
      _rmin = min(radarray) & _rmax = max(radarray)
      _lmin = min(lonarray) & _lmax = max(lonarray)
      ;if _lmin lt 10 and _lmax gt 350 and minmaxonly ne 2 then begin
      ;  lonarray[where(lonarray gt 180)] = lonarray[where(lonarray gt 180)]-360
      ;  _lmin = min(lonarray) & _lmax = max(lonarray)
      ;endif
    endelse
    if not keyword_exists(rmin) then begin
      filenames = image_name
      rmin = _rmin & rmax = _rmax
      lmin = _lmin & lmax = _lmax
      stretchmin = 0 & stretchmax = 0
      _keywords = keywords
      _et = et
      if keyword_set(do_radial_scale_adj) then begin
        _radial_scale_adj = radial_scale_adj
      endif
    endif else begin
      filenames = [ filenames, image_name ]
      rmin = [ rmin, _rmin ] & rmax = [ rmax, _rmax ]
      lmin = [ lmin, _lmin ] & lmax = [ lmax, _lmax ]
      stretchmin = [ stretchmin, 0 ] & stretchmax = [ stretchmax, 0 ]
      _keywords = [ _keywords, keywords ]
      if keyword_set(_et) then _et = [ _et, et ]
      if keyword_set(do_radial_scale_adj) then begin
        _radial_scale_adj = [ _radial_scale_adj, radial_scale_adj ]
      endif
    endelse

    if keyword_set(_nostars) then begin
      save, filenames, lmax, lmin, nostars, ring_npoints, rmax, rmin, $
            stretchmax, stretchmin, usehistogram, diffuse, basiconly, $
            order1, order2, fewerstars, fullhist, _keywords, $
            _radial_scale_adj, filename='stretch.sav'
    endif else begin
      if not keyword_exists(fewerstars) then fewerstars=1
      save, filenames, lmax, lmin, ring_npoints, rmax, rmin, $
            stretchmax, stretchmin, usehistogram, diffuse, basiconly, $
            order1, order2, fewerstars, fullhist, _keywords, $
            _radial_scale_adj, filename='stretch.sav'
    endelse 
    if keyword_set(_et) then save, _et, filename='et.sav'
  endfor
endif

end
