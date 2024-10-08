; Introduce Common block just in case it is necessary to use the CassImg
; apparatus to open the label and find a default pointing.  
if not keyword_set(prop_anal_common) then begin
  ; Load the COMMON blocks CalibrateGuiCommon and CalibrateGuiOpts. 
  ; Also load the current version of Cisscal in the variable CisscalVers
  @cisscal_common_caviar
  DebugFlag = 0
  prop_anal_common = 1
endif

; Compare identified propeller-candidate locations from adjacent images, 
; look for any that appear in multiple images
if not keyword_set(findfile('propellers.sav')) then begin
  if not keyword_set(propsuff) then propsuff = '.cray'
  crayfiles = findfile( '*'+propsuff )
  restore, 'stretch.sav'
  offsetfiles = findfile( '*.offset' )
  if keyword_set(crayfiles) then begin
    nf = n_elements(filenames)
    dot = strpos( filenames, '.IMG' )
    filestem = reform( (strmid( filenames, 0, dot ))[indgen(nf),indgen(nf)] )
    nc = n_elements(crayfiles)
    craydot = strpos( crayfiles, propsuff )
    crayfilestem = reform( (strmid( crayfiles, 0, craydot ))[indgen(nc),indgen(nc)] )
    for j=0,n_elements(crayfiles)-1 do begin
      foo = where( crayfilestem[j] eq filestem, count )
      if count eq 0 then stop, 'crayfile not found in filenames'
    endfor 
    @get_cam_params
    cspice_furnsh,getenv("CAVIAR_KERNELS")
    restore, 'et.sav'
    if keyword_set(timeoffset) then _et = _et + timeoffset
    for j=0,nf-1 do begin
      jjj = (where( filestem[j] eq $
                    strmid( filenames, 0, strlen(filestem[j]) ), count ))[0]
      if count ne 1 then stop, 'Not a single match to filenames.'
      cray = 0
      ncray = 0
      offsetfile = filestem[jjj]+'.offset'
      if keyword_set(findfile(offsetfile)) then restore, offsetfile else begin
        image_name = filestem[j] + '.IMG'
        ImageObj = OBJ_NEW('CassImg')
        ImageObj->ReadVic,image_name
        label=ImageObj->LabelArray()
        image_data,label,_et[jjj],epoch,exposure,cam_name,pmat,nl,found
        cmat = nacmat ## pmat
      endelse
      p2radec, cam_params, cmat, nl, [0,1023,1023,0], [0,0,1023,1023], cra, cdec
      p2ralon, cmat, _et[jjj], polera, poledec, sc, cra, cdec, cradius, clon
      if keyword_set(findfile(filestem[j]+propsuff)) then begin
        restore, filestem[j]+propsuff
        ncray = n_elements( cray[*,0] )
        p2radec, cam_params, cmat, nl, cray[*,0], cray[*,1], ra, dec
        p2ralon, cmat, _et[jjj], polera, poledec, sc, ra, dec, radius, lon
        if filestem[j]+propsuff eq crayfiles[0] then begin
          image = replicate( filestem[j]+'.IMG', ncray )
          prop_rad = radius
          prop_lon = lon
          prop_xy = cray
        endif else begin
          image = [ image, replicate( filestem[j]+'.IMG', ncray ) ]
          prop_rad = [ prop_rad, radius ]
          prop_lon = [ prop_lon, lon ]
          prop_xy = [ prop_xy, cray ]
        endelse 
      endif 
      if j eq 0 then begin
        corn_rad = rotate(cradius,1)
        corn_lon = rotate(clon,1)
      endif else begin
        corn_rad = [ corn_rad, rotate(cradius,1) ]
        corn_lon = [ corn_lon, rotate(clon,1) ]
      endelse
    endfor
    image_num = long(strmid( image, 1, 10 ))
    image_nums = long(strmid( filenames, 1, 10 )) ;image_num[ uniq(image_num) ]
    save, image, image_num, image_nums, prop_rad, prop_lon, prop_xy, $
          corn_rad, corn_lon, nf, filename='propellers.sav'
  endif else stop, 'No *'+propsuff+' files found.'
endif else restore, 'propellers.sav'
prop_lon_corr = prop_lon - sqrt(caviar_omega2(prop_rad)) * 180 / !dpi * $
                           ( image_num - image_num[0] )
if keyword_set(corn_lon) then begin
  if nf eq 1 then image_nums = reform(image_nums,1,1)
  corn_lon_corr = corn_lon - sqrt(caviar_omega2(corn_rad)) * 180 / !dpi * $
                             ( rebin(image_nums,nf,4) - image_num[0] )
endif
if keyword_set(decrease) then begin
  lons = [ max(prop_lon_corr), min(prop_lon_corr) ]
endif else begin
  lons = [ min(prop_lon_corr), max(prop_lon_corr) ]
endelse
if keyword_set(dolzr) then begin
  spawn, 'pwd', pwd
  lslashes = rstrpos( pwd, '/' )
  lslashes = [ lslashes, rstrpos( pwd, '/', lslashes ) ]
  if keyword_set(doubleplot) then if not keyword_set(qport) then port=1
  lzr, 'prop_anal_'+strmid( pwd, lslashes[1]+1, lslashes[0]-lslashes[1]-1 )+$
                    strlowcase(strmid( pwd, lslashes[0]+1, 1000 )), $
                    port=port, qport=qport
  !p.multi = [0,2,2]
  @plot_prepare
  plot_color
  reverse = 0
endif
if keyword_set(doubleplot) then begin
  !p.multi = [0,1,2]
  reverse = 0
  if 4 eq 5 then begin
    redo:
    reverse = 1
  endif
endif
if keyword_set(_yr) then yr = _yr
if keyword_set(_xr) then xr = _xr else xr = lons
if not keyword_set(tit) then tit = ''
plot, /nodata, lons, tkm([ min(prop_rad), max(prop_rad) ]), xr=xr, yr=yr, $
      /xs, /ys, xtit='Co-Rotating Longitude (!Uo!N)', ytit='Radius'+tkmtit(), $
      tit=tit
solid_small_circles
if keyword_set(reverse) then begin
  j1 = n_elements(image_nums)-1
  j2 = 0
  j3 = -1
endif else begin
  j1 = 0
  j2 = n_elements(image_nums)-1
  j3 = 1
endelse
clr = [ ctred(), ctcyan(), ctyellow(), ctpurple(), ctgreen(), ctblue() ]
nclr = n_elements(clr)
for j=j1,j2,j3 do begin
  foo = where( image_num eq image_nums[j], count )
  if count gt 0 then begin
    oplot, prop_lon_corr[foo], tkm(prop_rad[foo]), ps=8, co=clr[j mod nclr]
  endif
  if keyword_set(corn_lon_corr) then begin
    oplot, corn_lon_corr[j,[0,1,2,3,0]], tkm(corn_rad[j,[0,1,2,3,0]]), $
           l=1, co=clr[j mod nclr]
  endif
endfor

if keyword_set(doubleplot) then if reverse eq 0 then goto, redo
if keyword_set(dolzr) then clzr

end
