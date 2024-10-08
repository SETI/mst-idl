if not keyword_exists(fwhm) then fwhm = 0;1
if not keyword_exists(linesam) then linesam = 1
if not keyword_exists(printim) then printim = 1
dash = '-'

if not keyword_set(dirs) then begin
  dirs = [ '007/HIPHASE', '116/EQXSHADOW001', '116/EQXSHADOW005', $
           '116/SATELLORB', '116/EQXSHADOW013', '169/HIPHASE' ]
  basedir = '/home/borogove/iss/images/'
  ndirs = n_elements(dirs)
endif

alphabet = [ 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', $
             'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', $
             'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', $
             'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z' ]
cspice_furnsh, getenv("CAVIAR_KERNELS")
savefile = '~/idl/iss/impacts/impact_table.sav'
if keyword_set(only2005) then savefile = savefile + '.2005'
if keyword_set(only2009) then savefile = savefile + '.2009'
if keyword_set(findfile(savefile)) then restore, savefile else begin

  jg = -1l  ; global index parameter
  for k=0,ndirs-1 do begin
    restore, basedir + dirs[k] + '/stretch.sav'
    restore, basedir + dirs[k] + '/et.sav'
    restore, basedir + dirs[k] + '/prop_reproj_cea.sav'
    restore, basedir + dirs[k] + '/calculate_ea.sav'
    lastimage = ''
    abc = -1
    if dirs[k] eq '116/EQXSHADOW005' then j1 = 2 else j1 = 0
    j2 = n_elements(prop_reproj) - 1

    for j=j1,j2 do begin

      jg = jg + 1

      image_name = images[ prop_reproj[j].images ]
      sz = size(*( prop_reproj[j].rrpi ))
      mnrad = prop_reproj[j].radlon[0]
      mxrad = prop_reproj[j].radlon[1]
      mnlon = prop_reproj[j].radlon[2]
      mxlon = prop_reproj[j].radlon[3]
      radx = make_radi( mnrad, mxrad, mnlon, mxlon, sz, loni=lonx )
      radlon = [ interpol( radx, indgen(sz[2]), $
                           interpol(poly( indgen(sz[1]), slopefit[j,1,*] ), $
                                    indgen(sz[1]),cenbnd[0,j]) ), $
                 interpol( lonx, indgen(sz[1]), cenbnd[0,j] ) ]
      length = interpol( lonx, indgen(sz[1]), cenbnd[2,j] ) - $
               interpol( lonx, indgen(sz[1]), cenbnd[1,j] )
      length = length * !dpi/180 * radlon[0]
      adjlength = length + 35 - 200
      if dirs[k] eq '007/HIPHASE' then begin
        if j eq 0 or j eq 2 or j eq 3 then begin
          ;_width = [ 84052.0d0-84045, 0, 84034.5-84012.5, 84067-84057, 0 ]
          _width = [ 84052.0d0-84045, 0, 84032.5-84022.5, 84067-84057, 0 ]
          if abs(theta[j,1]) gt 0.02 then stop                                
          ; Subtract the smear length (200 km) but add back the conversion
          ; between apparent and actual smear. 
          theta[j,1] = atan( _width[j] / adjlength )
          theta[j,1] = round( theta[j,1]*180/!dpi / 10^(floor(alog10(theta[j,1]*180/!dpi))) )*10^(floor(alog10(theta[j,1]*180/!dpi)))*!dpi/180
          theta_sigma[j,1] = 0
        endif 
      endif 

      if image_name ne lastimage then begin
        abc = -1
        filestem = strmid( image_name, 0, strpos(image_name,'.IMG') )
        jjj = (where( filestem eq $
                      strmid( filenames, 0, strlen(filestem) ), count ))[0]
        if count ne 1 then stop, 'Not a single match to filenames.'
        restore, basedir + dirs[k] + '/' + filestem + '.offset'
        @get_cam_params
      endif
      abc = abc + 1
      get_ring, _et[jjj], radlon[0], 0, 0, polera, poledec, n_elements(foo), $
                impacts, 699l, lons=radlon[1]
      image_coords, impacts, cmat, [[0],[0],[0]], cam_params, nl, xy

      case dirs[k] of
        '007/HIPHASE': begin
          _match = 'C'+strtrim(j+1,2)
          if j eq 1 or j eq 4 then xyi=0 else xyi=1
          emission = 71.25
        end
        '116/EQXSHADOW001': begin
          _match = 'Bx '
          xyi = 1
          emission = 94.6
        end
        '116/EQXSHADOW005': begin
          _match = 'Cx '
          xyi = 1
          emission = 111.7
        end
        '116/SATELLORB': begin
          _match = 'Ax(1)'
          xyi = 1
          emission = 70.0
        end
        '116/EQXSHADOW013': begin
          _match = 'Ax(2)'
          xyi = 1
          emission = 73.6
        end
        '169/HIPHASE': begin
          _match = 'C6'
          xyi = 1
          emission = 102.2
        end
      endcase
      mu = abs(cos(emission*!dpi/180))

      if not keyword_set(impact_name) then begin
        impact_name = strmid(dirs[k],0,3)+'-'+$
                      string(jjj,fo='(I03)')+'-'+alphabet[abc]
        match = _match
        jgj = j
        _dirs = dirs[k]
        _image_name = image_name
        _radlon = radlon
        _length = length
        _xy = reform(xy)
        _ea = ea[j,xyi]
        _ea_sigma = ea_sigma[j,xyi]
        _eatau = eatau[j,xyi]
        _eatau_sigma = eatau_sigma[j,xyi]
        _maxif = maxif[j,xyi]
        _maxtau = maxtau[j,xyi]
        _theta = theta[j,xyi]
        _theta_sigma = theta_sigma[j,xyi]
        _mu = mu
        _adjlength = adjlength
      endif else begin
        impact_name = [ impact_name, strmid(dirs[k],0,3)+'-'+$
                        string(jjj,fo='(I03)')+'-'+alphabet[abc] ]
        match = [ match, _match ]
        jgj = [ jgj, j ]
        _dirs = [ _dirs, dirs[k] ]
        _image_name = [ _image_name, image_name ]
        _radlon = [ [_radlon], [radlon] ]
        _length = [ _length, length ]
        _xy = [ [_xy], [reform(xy)] ]
        _ea = [ _ea, ea[j,xyi] ]
        _ea_sigma = [ _ea_sigma, ea_sigma[j,xyi] ]
        _eatau = [ _eatau, eatau[j,xyi] ]
        _eatau_sigma = [ _eatau_sigma, eatau_sigma[j,xyi] ]
        _maxif = [ _maxif, maxif[j,xyi] ]
        _maxtau = [ _maxtau, maxtau[j,xyi] ]
        _theta = [ _theta, theta[j,xyi] ]
        _theta_sigma = [ _theta_sigma, theta_sigma[j,xyi] ]
        _mu = [ _mu, mu ]
        if dirs[k] eq '007/HIPHASE' then _adjlength = [ _adjlength, adjlength ]
      endelse

      lastimage = image_name

    endfor

  endfor
  nimpact = n_elements(jgj)

  save, impact_name, match, jgj, nimpact, _dirs, _image_name, _radlon, $
        _length, _xy, _ea, _ea_sigma, _eatau, _eatau_sigma, _maxif, _maxtau, $
        _theta, _theta_sigma, _width, _adjlength, _mu, filename=savefile

endelse 
if not keyword_set(mu1) then begin
  _eatau = _eatau * _mu
  _eatau_sigma = _eatau_sigma * _mu
  _maxtau = _maxtau * _mu
endif

olddir = ''
lastimage = ''
for jg1=0l,nimpact-1 do begin

  jg = jg1
  if jg1 eq 0 then begin
    if keyword_set(tex) then begin
    endif else begin
      print, 'Name   Image      [line,sample]   Radius (km)    '+$
             ' Longitude  Length   Orientation   Inferred Age (hr)   '+$
             'Peak I/F    Peak tau     Equiv. Area'
    endelse 
  endif

  if _dirs[jg] ne olddir and not keyword_set(nodash) then begin
    if keyword_set(tex) then begin
      if not keyword_set(hline) then print, '\hline'
      hline = 1
      ndash = 0
    endif else begin
      ndash = 159
    endelse 
    if keyword_set(ndash) then print, strjoin(replicate( dash, ndash ))
  endif
  olddir = _dirs[jg]

  ppfo = '(F5.1)'
  sgfo = '(F4.1)'
  rafo = '(I7)'
  rasgfo = '(I2)'
  lnfo = '(F7.2)'
  lnsgfo = '(F4.2)'
  if strmid(_dirs[jg],0,3) eq '116' then begin
    taufo = '(F5.3)'
    eafo = '(F7.2)'
    easgfo = '(F6.2)'
  endif else begin
    taufo = '(F7.5)'
    eafo = '(F7.3)'
    easgfo = '(F6.3)'
  endelse 
  extratxt = '                '
  extratxt1a = '                '
  extratxt1 = '                                  '
  if keyword_set(tex) then begin
    space2 = ' & '
    space3 = ' & '
    rdelim = ' & \hspace{-3cm} \ldelim\}{3}{0mm} '
    ldelim1 = ' & \hspace{0.8cm} \ldelim\{{3}{0mm} '
    ldelim2 = ' \hspace{2cm} \ldelim\{{3}{0mm} & '
    ;comma = ' & '
    comma = ','
    obrack = '[ '
    cbrack = ' ]'
    pm = ' & '
    pm1 = ' \pm '
    pm1_aux = '$'
    extratxt = ' & \multicolumn{2}{c}{} '
    extratxt1a = ' & '
    extratxt1 = ' & \multicolumn{2}{c}{} & \multicolumn{2}{c}{} '
    finish = '\\'
    it1 = ''
    it2 = ''
    deg = '^\circ'
  endif else begin
    space2 = '  '
    space3 = '      '
    rdelim = '  '
    ldelim1 = '  '
    ldelim2 = '  '
    comma = ','
    obrack = '['
    cbrack = ']'
    pm = '+-'
    pm1 = '+-'
    pm1_aux = ''
    finish = ''
    it1 = ''
    it2 = ''
    deg = ''
  endelse 
  if keyword_set(forpaper) then begin
    xyfo = '(I4)'
  endif else begin
    forpaper = 0
    xyfo = '(F6.1)'
  endelse 
  if not keyword_set(split) then split = 0
  if keyword_set(forpaper) and _image_name[jg] eq 'N1628669191_1_cal.IMG' and $
     jg ne (where( _image_name eq 'N1628669191_1_cal.IMG' ))[1] then begin
    if split eq 2 then goto, next
    if jg eq (where( _image_name eq 'N1628669191_1_cal.IMG' ))[0] then begin
      text = extratxt1a + extratxt1a + space2 + space2 + ldelim1
    endif else begin
      text = extratxt1a + extratxt1a + space2 + space2 + space2
    endelse
    goto, gotoorient
  endif 
  if _image_name[jg] ne lastimage then begin
    imname = strmid(_image_name[jg],0,11)
  endif else imname = '     "     '
  lastimage = _image_name[jg]
  if keyword_set(match) then text = match[jg] else text = impact_name[jg]
  text = text+space2+imname
  text = text+space2+obrack+string(_xy[0,jg],fo=xyfo)+comma+$
         string(_xy[1,jg],fo=xyfo)+cbrack
  if split eq 1 and imname eq 'N1721654859' then text = '%' + text
  if split eq 2 then goto, split2
  text = text+space2+string(_radlon[0,jg],fo=rafo)
  text = text+space2+string(_radlon[1,jg],fo=lnfo)+pm1_aux+deg+pm1_aux
  if forpaper eq 2 and _dirs[jg] eq '007/HIPHASE' then begin
    lonsm = 6
    dlonx = 0.001846
    text = text+space2+pm1_aux+string(_length[jg],fo='(I4)')+pm1+$
           string(lonsm*dlonx*!dpi/180*_radlon[0,jg],fo='(I2)')+pm1_aux
  endif else begin
    if _dirs[jg] eq '007/HIPHASE' then begin
      text = text+space2+string( round(_length[jg]/5)*5 + 35, fo='(I4)' )
    endif else begin
      sigfig = 10.^floor(alog10(_length[jg])-1)
      text = text+space2+string( round(_length[jg]/sigfig)*sigfig, fo='(I4)' )
    endelse 
  endelse
  gotoorient:
  if _theta_sigma[jg] ne 0 then begin
;    _theta_sigma[jg] = _theta_sigma[jg] > 0.01*!dpi/180
  endif
  if abs(_theta[jg]) gt 0.02 then begin
    nn = sqrt(caviar_omega2( _radlon[0,jg] ))*3600 ; radians/hour
    age = 2./3 / nn / tan(_theta[jg])
    age_sigma = 2./3 / nn / sin(_theta[jg])^2 * _theta_sigma[jg]
    if _theta_sigma[jg] eq 0 then begin
      orfo = '("\sim ",I4)' & orsgfo = ''
      _theta[jg] = round( _theta[jg] * 180/!dpi )*!dpi/180 + 0.001
    endif else if _theta_sigma[jg]*180/!dpi gt .95 then begin
      orfo = '(I4)' & orsgfo = '(I2)'
      _theta[jg] = round( _theta[jg] * 180/!dpi )*!dpi/180 + 0.001
      _theta_sigma[jg] = round( _theta_sigma[jg] * 180/!dpi )*!dpi/180 + 0.001
    endif else if _theta_sigma[jg]*180/!dpi gt .095 then begin
      orfo = '(F6.1)' & orsgfo = '(F3.1)'
    endif else if _theta_sigma[jg]*180/!dpi gt .0095 then begin
      orfo = '(F7.2)' & orsgfo = '(F4.2)'
    endif else if _theta_sigma[jg]*180/!dpi gt .00095 then begin
      orfo = '(F8.3)' & orsgfo = '(F5.3)'
    endif else stop
    text = text+space2+pm1_aux+$
           string(_theta[jg]*180/!dpi,fo=orfo)+deg
    if _theta_sigma[jg] ne 0 then begin
      text = text+pm1+string(_theta_sigma[jg]*180/!dpi,fo=orsgfo)+deg
    endif
    text = text+pm1_aux
    if forpaper ne 2 then begin
      if age_sigma eq 0 then begin
        agefo = '("\sim",I3)' & agesgfo = ''
        age = round(age)
      endif else if age_sigma gt .95 then begin
        agefo = '(I3)' & agesgfo = '(I2)'
        age = round(age)
        age_sigma = round(age_sigma)
      endif else if age_sigma gt .095 then begin
        agefo = '(F5.1)' & agesgfo = '(F4.1)'
      endif else if age_sigma gt .0095 then begin
        agefo = '(F6.2)' & agesgfo = '(F5.2)'
      endif else stop
      text = text+space3+pm1_aux+string(age,fo=agefo)
      if age_sigma ne 0 then begin
        text = text+pm1+string(age_sigma,fo=agesgfo)
      endif
      text = text+pm1_aux
    endif
  endif else begin
    if forpaper ne 2 then text = text+extratxt1a+extratxt1a
  endelse 
  if keyword_set(forpaper) and _image_name[jg] eq 'N1628669191_1_cal.IMG' and $
     jg ne (where( _image_name eq 'N1628669191_1_cal.IMG' ))[1] then begin
    if split eq 1 then begin
      if jg eq (where( _image_name eq 'N1628669191_1_cal.IMG' ))[0] then begin
        text = text + space2 + ' Central'
      endif else begin
        text = text + space2 + ' Inward/Leading'
      endelse 
    endif else begin
      if jg eq (where( _image_name eq 'N1628669191_1_cal.IMG' ))[0] then begin
        text = text + rdelim + extratxt1a + extratxt1a + ldelim2 + $
               ' Central Region'
      endif else begin
        text = text + extratxt1a + extratxt1a + space2 + space2 + $
               ' Inward/Leading Region'
      endelse 
    endelse
    goto, gotofinish
  endif 
  if split eq 1 then begin
    if imname eq 'N1628669191' then begin
      text = text+space2+' Outward/Trailing'
    endif
    goto, gotofinish
  endif
  split2:
  if keyword_set(_maxif[jg]) then begin
    ifmag = floor(alog10(_maxif[jg]))
    if _maxif[jg]/10.^ifmag gt 9.5 then ifmag = ifmag + 1
    iffo = '(F' + strtrim(-ifmag+2,2) + '.' + strtrim(-ifmag,2) + ')'
    taumag = floor(alog10(_maxtau[jg]))
    if _maxtau[jg]/10.^taumag gt 9.5 then taumag = taumag + 1
    taufo = '(F' + strtrim(-taumag+2,2) + '.' + strtrim(-taumag,2) + ')'
    text = text+space3+string(_maxif[jg],fo=iffo)+$
           space3+string(_maxtau[jg],fo=taufo)
    if _eatau_sigma[jg] gt 10 then begin
      text = text+space2+pm1_aux+string(round(_eatau[jg]/10),fo='(I6)')+'0'+$
             pm1+string(round(_eatau_sigma[jg]/10),fo='(I5)')+'0'+pm1_aux
    endif else if _eatau_sigma[jg] gt 1 then begin
      text = text+space2+pm1_aux+string(_eatau[jg],fo='(I7)')+pm1+$
             string(_eatau_sigma[jg],fo='(I6)')+pm1_aux
    endif else if _eatau_sigma[jg] gt 0.1 then begin
      text = text+space2+pm1_aux+string(_eatau[jg],fo='(F7.1)')+pm1+$
             string(_eatau_sigma[jg],fo='(F6.1)')+pm1_aux
    endif else if _eatau_sigma[jg] gt 0.01 then begin
      text = text+space2+pm1_aux+string(_eatau[jg],fo='(F7.2)')+pm1+$
             string(_eatau_sigma[jg],fo='(F6.2)')+pm1_aux
    endif else begin
      text = text+space2+pm1_aux+string(_eatau[jg],fo=eafo)+pm1+$
             string(_eatau_sigma[jg],fo=eafo)+pm1_aux
    endelse 
  endif else begin
    if keyword_set(forpaper) and keyword_set(tex) then text = '%' + text
    text = text+space3+' '+space3+' '+space3+' '
  endelse 
  if split eq 0 then case imname of
    'N1628669191':  text = text+space2+' Outward/Trailing Region'
    'N1721654859':  text = text+space2+' Blue (BL1) Filter'
    'N1721654965':  text = text+space2+' Infrared (IR3) Filter'
    else:  text = text+space2
  endcase
  if split eq 2 then case imname of
    'N1721654859':  text = text+space2+' Blue (BL1/CL2)'
    'N1721654965':  text = text+space2+' Infrared (CL1/IR3)'
    else:  text = text+space2+' Clear (CL1/CL2)'
  endcase
  gotofinish:
  noprint = [ 'N1628845250', 'N1628933453' ]
  if not ( keyword_set(forpaper) and $
           (where(imname eq noprint))[0] ne -1 ) then begin
    print, text + finish
  endif
  if _dirs[jg] eq '007/HIPHASE' or _dirs[jg] eq '116/EQXSHADOW013' or $
     not keyword_set(forpaper) then hline = 0
  next:

endfor

if keyword_set(tex) then begin
endif else begin
  print, strjoin(replicate( dash, ndash ))
endelse

end
