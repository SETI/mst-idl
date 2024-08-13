; Introduce Common block just in case it is necessary to use the CassImg
; apparatus to open the label and find a default pointing.  
if not keyword_set(prop_anal_common) then begin
  ; Load the COMMON blocks CalibrateGuiCommon and CalibrateGuiOpts. 
  ; Also load the current version of Cisscal in the variable CisscalVers
  @cisscal_common_caviar
  DebugFlag = 0
  prop_anal_common = 1
endif

; For identified propeller-candidate locations in any image, make a 
; reprojected image for all corresponding locations in all images. 
if keyword_set(example) or keyword_set(figure) then begin
  savefile = '~/idl/iss/propellers/prop_table.sav'
  if keyword_set(encke) then savefile = savefile + '.encke'
  if keyword_set(redge) then savefile = savefile + '.redge'
  if not keyword_set(_radlon) then restore, savefile
  dirs = _dirs[ uniq(_dirs) ]
  ndirs = n_elements(dirs)
  for k=0,ndirs-1 do begin
    restore, '/home/borogove/iss/images/'+dirs[k]+'/prop_reproj.sav'
    if keyword_set(redge) and dirs[k] ne '013/AZSCNHIPH001' then begin
      restore, '/home/borogove/iss/images/'+dirs[k]+'/prop_reproj_redge.sav'
    endif 
    if k eq 0 then begin
      _prop_reproj = prop_reproj[jgj[where( _dirs eq dirs[k] )]]
      _images = images[jgj[where( _dirs eq dirs[k] )]]
    endif else begin
      _prop_reproj = [ _prop_reproj, $
                       prop_reproj[jgj[where( _dirs eq dirs[k] )]] ]
      _images = [ _images, images[jgj[where( _dirs eq dirs[k] )]] ]
    endelse 
  endfor 
  prop_reproj = _prop_reproj
  images = _images
  if keyword_set(example) then begin
    foo = [ (where( prop_name eq 'SOI-41-A' ))[0], $
            (where( prop_name eq '013-08-B' ))[0], $
            (where( prop_name eq '013-12-H' ))[0], $
            (where( prop_name eq '013-20-A' ))[0], $
            (where( prop_name eq '028-34-J' ))[0], $
            (where( prop_name eq '032-45-A' ))[0], $
            (where( prop_name eq '046-11-C' ))[0], $
            (where( prop_name eq '046-12-A' ))[0] ]
    nprop = n_elements(foo)
    figure = 1
  endif else begin
    foo = where( images eq images, nprop )
  endelse
  duplicates = prop_reproj.duplicates
  text = ''
  j = 0l
  k = 0l
  if not keyword_exists(onepage) then onepage = 1
  cantmatch = 1
  goto, nextpage1
endif
if not keyword_set(findfile('propellers.sav')) then begin
  stop, '.run prop_anal first'
endif else restore, 'propellers.sav'
if not keyword_set(findfile('prop_reproj.sav')) then begin
  restore, 'et.sav'
  restore, 'stretch.sav'
  if keyword_set(timeoffset) then _et = _et + timeoffset
  cspice_furnsh, getenv("CAVIAR_KERNELS")
  ;images = image[uniq( image, sort(image) )]
  images = strarr(nf)
  for j=0,nf-1 do images[j] = (filenames[where( strmid(filenames,1,10) eq $
                                               image_nums[j] )])[0]
  x = 5;3
  spawn, 'pwd', dir
  lastslash = rstrpos( dir[0], '/' )
  lastslash2 = rstrpos( dir[0], '/', lastslash )
  dir = strmid( dir[0], lastslash2+1, 100 )
  case dir of
    'SOI/SOISPTURN': drcase = 1
    '007/AZSCNLOPH': drcase = 8
    '007/HIPHASE': drcase = 9
    '007/LPHRLFMOV': drcase = 8
    '007/LATPHASE': drcase = 8
    '008/LPHRLFMOV': drcase = 8
    '009/RETARG': drcase = 8
    '012/NAMEORBIT': drcase = 8
    '013/ALPSCOOCC': drcase = 2
    '013/AZSCNHIPH001': drcase = 4
    '028/RDHRESSCN': drcase = 4
    '028/HIPHNAC': drcase = 8
    '029/FMOVIE': drcase = 8
    '030/FMOVIE': drcase = 8
    '030/AZDKMRHPH': drcase = 8
    '031/RDHRCOMP': drcase = 3
    '031/SUBML20MP': drcase = 6
    '032/RDHRCOMP': drcase = 3
    '033/HPMRLFMOV': drcase = 11
    '033/FMOVIE': drcase = 8
    '034/HIPHAMOVD': drcase = 8
    '035/AZSCNLOPH': drcase = 8;5
    '036/FMOVIE001': drcase = 8
    '036/FMOVIE002': drcase = 8
    '037/WNPLUNLT': drcase = 8
    '039/FMOVIE': drcase = 8
    '041/FMOVIE002': drcase = 8
    '041/FMOVIE001': drcase = 8
    '042/RDMRSCNMP': drcase = 5
    '043/AZSCAN': drcase = 5
    '044/FMOVIE': drcase = 8
    '045/SATELLORB': drcase = 8
    '045/RDCOLSCNB_part': drcase = 10
    '046/RDHRESSCN': drcase = 2
    '053/LPHRDFMOV': drcase = 8
    '054/SATELLORB': drcase = 8
    '055/FMOVIE': drcase = 8
    '055/LPMRDFMOV': drcase = 8
    '056/RDHRESSCN': drcase = 8
    '057/FMOVIE': drcase = 8
    '061/SATELLORB': drcase = 8
    '064/AZSCAN': drcase = 5
    '065/LPHRLFMOV': drcase = 8
    '068/RETARGEMR': drcase = 8
    '070/PAZSCN001': drcase = 5
    '070/PAZSCN002': drcase = 7
    '071/SATELLORB': drcase = 8
    '080/SATELLORB': drcase = 8
    '081/AZSCANDRK': drcase = 6
    '090/SHRTMOV': drcase = 8
    '092/SATELLORB': drcase = 8
    '098/URBETCRU': drcase = 8
    '102/RETARMRLP': drcase = 8
    '105/FMOVIE003': drcase = 8
    '110/FMOVIE': drcase = 8
    '114/SATELLORB': drcase = 8
    '115/FMOVIEEQX': drcase = 8
    '116/EQXSHADOW001': drcase = 11
    '116/EQXSHADOW005': drcase = 8;7
    '116/SATELLORB': drcase = 12;14;10
    '116/EQXSHADOW013': drcase = 10
    '132/PROPELLR': drcase = 7
    '134/PROPELLR': drcase = 7
    '167/PROPSURVY': drcase = 13
    '168/FMOVIE001': drcase = 8
    '168/PROPRETRG': drcase = 15
    '169/HIPHASE': drcase = 12
    '172/FMOVIE001': drcase = 15
    '172/EGAPMOVMP': drcase = 15
    '172/PROPRETRG': drcase = 8
    '173/PROPRETRG': drcase = 8
    '174/FRSTRCHAN': drcase = 15
    '174/PROPSURVY': drcase = 8
    '175/PROPRETRG': drcase = 8
    '176/FMOVIE': drcase = 8
    '177/FMOVIE': drcase = 8
    '178/EGAPMOVMP': drcase = 8
    '179/FMOVIE': drcase = 8
    '179/PROPRETRG': drcase = 8
    '180/FMOVIE' : drcase = 8
    '180/PROPSURVY' : drcase = 8
    '181/ENCKEMOVE' : drcase = 8
    '181/FMOVIE' : drcase =8
    '181/PROPSURVY' : drcase = 8
    '183/FMOVIE' : drcase = 8
    '184/FMOVIE002' : drcase = 8
    '184/FMOVIE001' : drcase = 8
    '185/PROPRETRG': drcase = 8
    '186/PROPRETRG' :drcase = 8
    '188/PROPRETRG' : drcase = 8
    '189/PROPSURVY': drcase = 8
    '190/PROPRETRG' : drcase =8
    '191/PROPRETRG' : drcase = 8
    '192/PROPRETRG' : drcase = 8
    '193/FMOVIE' : drcase = 8
    '193/PROPRETRG' : drcase = 8
    '194/PROPRETRG' : drcase = 8
    '196/PROPRETRG' : drcase = 8
    '196/FMOVIE003' : drcase = 8
    '196/FMOVIE004' : drcase = 8
    '196/FMOVIE005' : drcase = 8
    '196/FMOVIE006' : drcase = 8
    '197/FMOVIE007' : drcase = 8
    '197/FMOVIE002' : drcase = 8
    '197/PROPRETRG': drcase = 8
    '198/PROPRETRG002' : drcase = 8
    '198/PROPRETRG003' : drcase = 10
    '199/PROPRETRG' : drcase = 8
    '199/FMOVIE' : drcase = 8
    '201/PROPRETRG' : drcase = 9
    '201/FMOVIE': drcase = 8
    '202/PROPRETRG' : drcase = 9
    '205/FMOVIE' : drcase = 9
    '207/FMOVIE' : drcase = 8
    '208/PROPRETRG' : drcase = 8
    '211/PROPRETRG' : drcase = 8
    '212/PROPRETRG' : drcase = 9
    '232/PROPRETRG' : drcase = 8
    '233/PROPSURVY' : drcase = 9
    '234/PROPRETRG' : drcase = 6
    '235/PROPRETRG' : drcase = 10
    '237/PROPRETRG' : drcase = 8
    '239/PROPRETRG' : drcase = 8
    '240/PROPRETRG' : drcase = 8
    '241/PROPSURVY' : drcase = 7
    '243/PROPRETRG' : drcase = 7
    '245/PROPRETRG' : drcase = 7
    '247/PROPRETRG' : drcase = 7
    '251/PROPRETRG' : drcase = 7
    '253/PROPRETRG' : drcase = 6
    '255/PROPRETRG' : drcase = 6
    '256/PROPRETRG' : drcase = 6
    '260/PROPRETRG' : drcase = 6
    '262/PROPRETRG' : drcase = 6
    '270/SUPRHRESL' : drcase = 2
    '270/SUPRHRESU' : drcase = 2
    '265/PROPRETRG' : drcase = 6
    '268/PROPRETRG' : drcase = 6
    '269/PROPRETRG' : drcase = 6
    '270/PROPRETRG' : drcase = 6
    '274/PROPRETRG' : drcase = 6
    '277/PROPRETRG' : drcase = 6
    '282/PROPRETRG' : drcase = 6
    '283/PROPRETRG' : drcase = 6
    '286/PROPRETRG' : drcase = 6
    '287/PROPRETRG' : drcase = 6
    '292/PROPRETRG' : drcase = 6
    '293/PROPRETRG' : drcase = 6
    else: drcase = 4
  endcase 
  case drcase of
    1: begin
      ; SOI/SOISPTURN
      dr = 1.8d0
      dl = 0.002d0
      propsuff = '.cray1'
    end 
    2: begin
      ; 013/ALPSCOOCC and 046/RDHRESSCN
      dr = 10.0d0
      dl = 0.005d0
    end 
    3: begin
      ; 031/RDHRCOMP and 032/RDHRCOMP
      dr = 12.5d0
      dl = 0.009d0
    end 
    4: begin
      ; 028/RDHRESSCN and 013/AZSCNHIPH001 and Default
      dr = 16.0d0
      dl = 0.015d0
    end 
    5: begin
      ; 035/AZSCNLOPH and 042/RDMRSCNMP and 043/AZSCAN and 064/AZSCAN
      dr = 16.0d0
      dl = 0.05d0
    end 
    6: begin
      ; 031/SUBML20MP and 081/AZSCANDRK
      dr = 30.0d0
      dl = 0.08d0
    end 
    7: begin
      ; 070/PAZSCN002 and 132/PROPELLR and 134/PROPELLR
      dr = 30.0d0
      dl = 0.2d0
    end 
    8: begin
      ; 007/AZSCNLOPH and 007/LPHRLFMOV and 007/LATPHASE and 008/LPHRLFMOV
      ; and 009/RETARG and 012/NAMEORBIT and 028/HIPHNAC and 029/FMOVIE
      ; and 030/FMOVIE and 030/AZDKMRHPH and 033/FMOVIE and 034/HIPHAMOVD
      ; and 035/AZSCNLOPH and 036/FMOVIE001 and 036/FMOVIE002 and 037/WNPLUNLT
      ; and 039/FMOVIE and 041/FMOVIE002 and 041/FMOVIE001 and 043/AZSCAN
      ; and 044/FMOVIE and 045/SATELLORB and 053/LPHRDFMOV and 054/SATELLORB
      ; and 055/FMOVIE and 055/LPMRDFMOV and 056/RDHRESSCN and 057/FMOVIE
      ; and 061/SATELLORB and 065/LPHRLFMOV and 068/RETARGEMR and 070/PAZSCN002
      ; and 071/SATELLORB and 080/SATELLORB and 090/SHRTMOV and 092/SATELLORB
      ; and 098/URBETCRU and 102/RETARMRLP and 105/FMOVIE003 and 110/FMOVIE
      ; and 115/FMOVIEEQX and 116/EQXSHADOW005 (impact) and 167/PROPSURVY
      ; and 168/FMOVIE001 and 172/PROPRETRG and 173/PROPRETRG and 174/PROPSURVY
      ; and 175/PROPRETRG and 176/FMOVIE and 177/FMOVIE and 178/EGAPMOVMP
      ; and 179/FMOVIE and 179/PROPRETRG and 181/PROPSURVY and 180/FMOVIE
      ; 185/PRORETRG and 189/PROPSURVY and 194/PROPRETRG and 193/FMOVIE
      ; 183/FMOVIE and 193/PROPRETRG and 191/PROPRETRG and 181/FMOVIE
      ; 190/PROPRETRG and 186/PROPRETRG and 188/PROPRETRG and 184/FMOVIE002 and
      ; 184/FMOVIE001 and 196/PROPRETRG and 196/FMOVIE003 and 196/FMOVIE004
      ; 196/FMOVIE005 and 196/FMOVIE006 and 197FMOVIE002 and 197FMOVIE007
      ; 198/PROPRETRG002 and 198/PROPRETRG003 and 199/PROPRETRG and 199/FMOVIE
      ; 197/PROPRETRG and 201/PROPRETRG and 201/FMOVIE and 202/PROPRETRG
      ; 205/FMOVIE and 207/FMOVIE and 208/PROPRETRG and 232/PROPRETRG and
      ; 233/PROPSURVY and 234/PROPRETRG
      dr = 50.0d0
      dl = 0.5d0
    end 
    9: begin
      ; 007/HIPHASE (impact)
      dr = 200.0d0
      dl = 0.3d0
    end 
    10: begin
      ; 045/RDCOLSCNB_part and 116/SATELLORB (impact, formerly), 
      ; 116/EQXSHADOW013 (impact)
      dr = 200.0d0
      dl = 1.6d0
    end 
    11: begin
      ; 033/HPMRLFMOV
      dr = 800.0d0
      dl = 2.5d0
    end 
    12: begin
      ; 116/SATELLORB (non-impact) and 169/HIPHASE (impact)
      dr = 300.0d0
      dl = 0.3d0
    end 
    13: begin
      ; 167/PROPSURVY
      dr = 25.0d0
      dl = 2.5d0
      dl1 = 0.2d0
      dl1_img = 145
    end 
    14: begin
      ; 116/SATELLORB (impact)
      dr = 300.0d0
      dl = 1.6d0
    end 
    15: begin
      ; 172/FMOVIE001 and 172/EGAPMOVMP and 174/FRSTRCHAN
      dr = 100.0d0
      dl = 25.0d0
    end 
  endcase 
  resfac = 1;4
  for j=0l,nf-1 do begin
    image_name = images[j]
    filestem = strmid( image_name, 0, strpos(image_name,'.IMG') )
    if keyword_set(bksub) then begin
      if keyword_set(findfile(filestem+'.bksub')) then begin
        restore, filestem+'.bksub'
        file_used = filestem+'.bksub'
      endif else goto, nobksub
      rawim = bksub_img
    endif else begin
      nobksub:
      rawim = read_vicar( image_name )
      file_used = image_name
    endelse
    print, strtrim(j,2)+' / '+strtrim(nf,2)+'   '+file_used
    fix_badlines, image_name, rawim
    ; These files must exist, or prop_anal would not have run
    jjj = (where( filestem eq $
                  strmid( filenames, 0, strlen(filestem) ), count ))[0]
    if keyword_set(dl1_img) then begin
      if jjj ge dl1_img then dl = dl1
    endif 
    if count ne 1 then stop, 'Not a single match to filenames.'
    cray = 0
    if not keyword_set(propsuff) then propsuff = '.cray'
    if keyword_set(findfile(filestem+propsuff)) then restore, filestem+propsuff
    @get_cam_params
    offsetfile = filestem+'.offset'
    if keyword_set(findfile(offsetfile)) then restore, offsetfile else begin
      image_name = filestem + '.IMG'
      ImageObj = OBJ_NEW('CassImg')
      ImageObj->ReadVic,image_name
      label=ImageObj->LabelArray()
      image_data,label,_et[jjj],epoch,exposure,cam_name,pmat,nl,found
      cmat = nacmat ## pmat
    endelse
    prop_lon_corr = prop_lon - sqrt(caviar_omega2(prop_rad)) * 180 / !dpi * $
                               ( image_num - image_nums[j] )
    prop_lon_corr = fix_angles( prop_lon_corr, /deg, /to360 )
    foo = where( image eq images[j], count )
    get_ring, _et[jjj], prop_rad, 0, 0, polera, poledec, $
              n_elements(prop_rad), props, 699l, lons=prop_lon_corr
    image_coords, props, cmat, [[0],[0],[0]], cam_params, nl, coords
    if count gt 0 then if max(coords[foo]-cray) gt 1 then begin
        stop, 'Mismatch between coords and cray'
    endif
    foo = where( coords[*,0] ge -x and coords[*,0] lt 1023+x and $
                 coords[*,1] ge -x and coords[*,1] lt 1023+x, count )
    if count gt 0 then begin
      duplicates = lonarr(count)
      k = 0l
      while k le count-1 do begin
        ; Check for duplicates
        foo1 = where( prop_rad[foo] gt prop_rad[foo[k]] - dr and $
                      prop_rad[foo] lt prop_rad[foo[k]] + dr and $
                      prop_lon_corr[foo] gt prop_lon_corr[foo[k]] - dl and $
                      prop_lon_corr[foo] lt prop_lon_corr[foo[k]] + dl and $
                      foo ne foo[k] and image[foo] ne image[foo[k]], count1 )
        if count1 le 1 then foo1 = foo1[0] else begin
          print, k, foo1
          print, foo[[k,foo1]]
          print, prop_rad[foo[[k,foo1]]]
          print, prop_lon_corr[foo[[k,foo1]]]
;          stop
        endelse
        if count1 gt 0 then begin
          foostay = where( image[foo[[k,foo1]]] eq image_name, countstay )
          foogo = where( image[foo[[k,foo1]]] ne image_name, countgo )
;          if countstay+countgo ne 2 then stop, 'error1'
          if countstay gt 0 then tostay = ([k,foo1])[foostay[0]]
          if countgo gt 0 then begin
            togo = ([k,foo1])[foogo[0]]
            if countstay gt 0 then duplicates[tostay] = foo[togo]
            duplicates = vec_remove( duplicates, where( foo eq foo[togo] ) )
            foo = vec_remove( foo, where( foo eq foo[togo] ) )
            count = count - 1
            if k eq togo then k = k - 1
          endif 
        endif
        k = k + 1
      endwhile
    endif
    for k=0,count-1 do begin
      if k mod 30 eq 0 then print, strtrim(k,2)+' / '+strtrim(count,2)
      mnrad = prop_rad[foo[k]] - dr
      mxrad = prop_rad[foo[k]] + dr
      mnlon = prop_lon_corr[foo[k]] - dl
      mxlon = prop_lon_corr[foo[k]] + dl
      rx = 0 & ry = 0
      caviar_reproject, byte(rawim*0), rawim, mnrad, mxrad, mnlon, mxlon, $
                        _et[jjj], polera, poledec, cam_params, nl, cmat, $
                        [[0],[0],[0]], ry, rx, 0, resfac, /silent, /noplot, $
                        raw_reprojected_image=rrpi
      propxy = [ rx/2, ry/2 ]
      foo1 = max(where( rebin(rrpi,rx,1) ne 0 ))
      if foo1 ne rx-1 then begin
        radx = make_radi( mnrad, mxrad, mnlon, mxlon, size(rrpi), loni=lonx )
        mxlon = lonx[foo1]
        rrpi = rrpi[0:foo1,*]
        rx = foo1 + 1
      endif 
      foo1 = min(where( rebin(rrpi,rx,1) ne 0 ))
      if foo1 ne 0 then begin
        radx = make_radi( mnrad, mxrad, mnlon, mxlon, size(rrpi), loni=lonx )
        mnlon = lonx[foo1]
        rrpi = rrpi[foo1:rx-1,*]
        rx = rx - foo1
        propxy[0] = propxy[0] - foo1
     endif 
      if not keyword_set(prop_reproj) then begin
        prop_reproj = { images:j, prop:foo[k], $
                        primary:(image[foo[k]] eq image_name), $
                        duplicates:duplicates[k], $
                        xy:[rx,ry,round([coords[foo[k],0],coords[foo[k],1]]),$
                            round(propxy)], $
                        radlon:[mnrad,mxrad,mnlon,mxlon,prop_rad[foo[k]],$
                                prop_lon_corr[foo[k]]], $
                        rrpi:ptr_new(rrpi) }
      endif else begin
        prop_reproj = [ prop_reproj, { images:j, prop:foo[k], $
                        primary:(image[foo[k]] eq image_name), $
                        duplicates:duplicates[k], $
                        xy:[rx,ry,round([coords[foo[k],0],coords[foo[k],1]]),$
                            round(propxy)], $
                        radlon:[mnrad,mxrad,mnlon,mxlon,prop_rad[foo[k]],$
                                prop_lon_corr[foo[k]]], $
                        rrpi:ptr_new(rrpi) } ]
      endelse
    endfor
  endfor
  save, images, prop_reproj, bksub, filename='prop_reproj.sav'
endif else restore, 'prop_reproj.sav'
duplicates = 0

if keyword_set(nomatch) or keyword_set(cantmatch) then begin
  foo = where( prop_reproj.primary eq 1, nprop )
  foo1 = where( prop_reproj.primary eq 0, count )
  mask1 = lonarr( n_elements(prop_rad) ) - 1
  if count gt 0 then mask1[ prop_reproj[foo1].prop ] = foo1
  mask2 = lonarr( n_elements(prop_reproj) ) - 1
  mask2[foo] = mask1
  if keyword_set(nomatch) then begin
    foo = where( prop_reproj.primary eq 1 and prop_reproj.duplicates eq 0 and $
                 mask2 ne -1, nprop )
    duplicates = mask2[foo]
  endif else begin
    foo = where( prop_reproj.primary eq 1 and prop_reproj.duplicates eq 0 and $
                 mask2 eq -1, nprop )
    duplicates = 1
  endelse
endif
if keyword_set(match) then begin
  foo = where( prop_reproj.primary eq 1 and $
               prop_reproj.duplicates ne 0, nprop )
  duplicates = lonarr(nprop) - 1
  k = 0l
  while k le nprop-1 do begin
    foo1 = (where( prop_reproj[foo].duplicates eq prop_reproj[foo[k]].prop, $
                   count1 ))[0]
    if count1 gt 1 then stop, 'error3.'
    if count1 gt 0 then begin
      duplicates[k] = foo[foo1]
      duplicates = vec_remove( duplicates, where( foo eq foo[foo1] ) )
      foo = vec_remove( foo, where( foo eq foo[foo1] ) )
      nprop = nprop - 1
    endif
    k = k + 1
  endwhile
endif
if not keyword_set(duplicates) then stop, 'Please set either match=1 or nomatch=1 or cantmatch=1'
if keyword_set(cantmatch) then text='One marked feature, no corresponding location (can''t match)'
if keyword_set(nomatch) then text='One marked feature, corresponding location not marked (no match)'
if keyword_set(match) then text='Two marked features (match)'
j = 0l
k = 0l
if not keyword_exists(onepage) then onepage = 1
nextpage1:
if keyword_set(dolzr) then begin
  spawn, 'pwd', pwd
  lslashes = rstrpos( pwd, '/' )
  lslashes = [ lslashes, rstrpos( pwd, '/', lslashes ) ]
  if keyword_set(cantmatch) then suff='_cantmatch'
  if keyword_set(nomatch) then suff='_nomatch'
  if keyword_set(match) then suff='_match'
  if keyword_set(figure) then suff=''
  if keyword_set(encke) then suff='_encke'
  if keyword_set(redge) then suff='_redge'
  if keyword_set(onepage) then suff=suff+'_'+strtrim(k,2)
  if keyword_set(example) then begin
    lzr, 'prop_reproj_example'
    plot_color
    loadct, 0
  endif else if keyword_set(figure) then begin
    if keyword_set(lastland) and k eq 4 then begin
      lzr, 'prop_reproj_figure_land' + suff
    endif else if keyword_set(firstland) and k eq 0 then begin
      lzr, 'prop_reproj_figure_land' + suff
    endif else begin
      lzr, 'prop_reproj_figure' + suff, /port
    endelse 
  endif else begin
    lzr, 'prop_reproj_' + $
         strmid( pwd, lslashes[1]+1, lslashes[0]-lslashes[1]-1 ) + $
         strlowcase(strmid( pwd, lslashes[0]+1, 1000 )) + suff
  endelse 
  @plot_prepare
endif else begin
  window, ys=!d.x_size * 9.5 / 7
endelse
nextpage2:
; Four columns, with 5% of the total width as a margin between them, in cm
if keyword_set(example) then begin
  xmargin = 0.02
  ncol = 4
endif else if keyword_set(figure) then begin
  if keyword_set(lastland) and k eq 4 then begin
    xmargin = 0.02
    ncol = 8
  endif else if keyword_set(firstland) and k eq 0 then begin
    xmargin = 0.03
    ncol = 7
  endif else begin
    xmargin = 0.03
    ncol = 5
  endelse
endif else begin
  xmargin = 0.05
  ncol = 4
endelse
x0 = !d.x_size * (1+xmargin)/ncol * indgen(ncol)
xcen = x0 + !d.x_size * ( (1+xmargin)/ncol - xmargin )/2
xsmax = !d.x_size * ( (1+xmargin)/ncol - xmargin )
; Five rows, with 3% of the total width as a margin between them, in cm
if keyword_set(example) then begin
  ymargin = 0.03
  nrow = 4
endif else if keyword_set(figure) then begin
  if keyword_set(lastland) and k eq 4 then begin
    ymargin = 0.03
    nrow = 7
  endif else if keyword_set(firstland) and k eq 0 then begin
    ymargin = 0.03
    nrow = 7
  endif else begin
    ymargin = 0.02
    nrow = 10
  endelse
endif else begin
  ymargin = 0.03
  nrow = 5
endelse
;y0 = !d.y_size * (1+ymargin)/nrow * [4,3,2,1,0];reverse(indgen(nrow))
y0 = !d.y_size * (1+ymargin)/nrow * reverse(indgen(nrow))
ycen = y0 + !d.y_size * ( (1+ymargin)/nrow - ymargin )/2
if keyword_set(example) then ycen = ycen + [300,1200,0,0]
ysmax = !d.y_size * ( (1+ymargin)/nrow - ymargin )
rownum = -1
help,xsmax,ysmax,ncol,nrow
nextprop:
m = 0l
rownum = rownum + 1
for m=0,ncol-1 do begin
  if keyword_set(cantmatch) then begin
    if m ne 0 then j = j + 1
    if j eq nprop then goto, finish1
    jj = foo[j]
  endif else begin
    if m ne 0 and (m mod 2) eq 0 then j = j + 1
    if j eq nprop then goto, finish1
    if (m mod 2) eq 0 then jj = foo[j] else jj = duplicates[j]
  endelse
  if jj ge 0 then begin
    if not keyword_exists(xtruncate) then xtruncate = 0
    if keyword_set(encke) and xtruncate lt 2 then begin
      truncate_these = [ '007-087-A', '007-173-A', '007-199-A', '036-069-A', $
                         '036-070-A', '037-001-A', $
                         '037-002-A', '039-009-A', '039-140-A', '041-088-A', $
                         '041-089-A', '041-001-A', '041-002-A', '041-003-A', $
                         '041-004-A', '055-061-A', '055-062-A', '055-063-A', $
                         '056-003-A', '057-005-A', '057-006-A', '057-007-A', $
                         '061-045-A', '061-047-A', '070-021-A', '071-002-A', $
                         '092-004-A', '092-005-A', $
                         '098-000-A', '098-001-A', '098-002-A', $
                         '110-087-A', '110-088-A', '114-001-A', '114-015-A' ]
      if (where( prop_name[foo[jj]] eq truncate_these )) ne -1 then begin
        xtruncate = 1
      endif else xtruncate = 0
    endif 
    if ( keyword_set(_edgebnd) or keyword_set(__pp) ) and $
       keyword_set(xtruncate) then begin
      if keyword_set(_edgebnd) then begin
        bnd = _edgebnd[[0,3],jj]
      endif else if keyword_set(__pp) then begin
        bnd = __pp[4,jj] + [-1,1]*( __pp[6,jj] + __pp[2,jj] )
      endif else stop
      if keyword_set(encke) then truncfac = 5 else truncfac = 0.25
      len = ( bnd[1] - bnd[0] )
      bnd = round([ (bnd[0]-len*truncfac)>0, $
                    (bnd[1]+len*truncfac)<(prop_reproj[jj].xy[0]-1) ])
      *(prop_reproj[jj].rrpi) = (*(prop_reproj[jj].rrpi))[bnd[0]:bnd[1],*]
      prop_reproj[jj].xy[0] = bnd[1]-bnd[0]+1
    endif 
    aspectratio = float((prop_reproj[jj].xy)[0]) / (prop_reproj[jj].xy)[1]
    multim = 0
    arfac = 6
    if aspectratio gt xsmax/ysmax*arfac then begin
      num = fix( aspectratio/xsmax*ysmax/arfac ) < ncol
      xs = xsmax + x0[1]*num
      ys = xs/aspectratio
      if m ge ncol-num then begin
        m = 0
        if rownum ge nrow-1 then begin
          goto, finishpage
        endif else rownum = rownum + 1
      endif
      x = x0[m]
      y = ycen[rownum] - ys/2
      multim = m + indgen(num+1)
      m = m + num
    endif else if aspectratio gt xsmax/ysmax then begin
      xs = xsmax
      ys = xsmax/aspectratio
      x = x0[m]
      y = ycen[rownum] - ys/2
    endif else begin
      xs = ysmax*aspectratio
      ys = ysmax
      x = xcen[m] - xs/2
      y = y0[rownum]
    endelse
    if n_tags(prop_reproj) eq 8 then begin
      rrpi = *(prop_reproj[jj].rrpi)
      rrpi_hist = *(prop_reproj[jj].rrpi4)
    endif else begin
      rrpi = *(prop_reproj[jj].rrpi)
      rrpi_hist = *(prop_reproj[jj].rrpi)
    endelse
    f = 10
    foo1 = wher( rrpi eq 0, count )
    mask = byte(rrpi*0) + 1
    if count gt 0 then begin 
      box = 1;5
      if prop_reproj[jj].xy[2] gt 850 then box = 0
      for k=0l,count-1 do begin
        mask[ (foo1[0,k]-box)>0 : (foo1[0,k]+box)<(prop_reproj[jj].xy[0]-1), $
              (foo1[1,k]-box)>0 : (foo1[1,k]+box)<(prop_reproj[jj].xy[1]-1) ]=0
      endfor
      rrpi[where( mask eq 0 )] = median(rrpi[where( mask eq 1 )])
      if n_tags(prop_reproj) eq 8 then stop, 'rrpi_hist ne rrpi' else begin
        rrpi_hist[where( mask eq 0 )] = median(rrpi_hist[where( mask eq 1 )])
      endelse 
    endif
    if __subtractavg[j] then begin
      rrpi = fit_propellers4_subtractavg( rrpi, rrpi, mask, $
                                          [0,(prop_reproj[jj].xy)[0:1]] )
      if n_tags(prop_reproj) eq 8 then stop, 'rrpi_hist ne rrpi' else begin
        rrpi_hist = fit_propellers4_subtractavg( rrpi_hist, rrpi_hist, mask, $
                                                 [0,(prop_reproj[jj].xy)[0:1]] )
      endelse 
    endif 
    ;stmin = median(rrpi) - stddev(rrpi)*f
    ;stmax = median(rrpi) + stddev(rrpi)*f
    ;hist = histogram(rrpi,nbins=300,locations=locations)
    ;gfit = gaussfit( locations, hist, aa )
    ;stmin = aa[1] - aa[2]*f
    ;stmax = aa[1] + aa[2]*f
    if not keyword_exists(hthresh) then begin
      if keyword_set(figure) then hthresh = 0 else hthresh = 1;5
    endif 
    run_histogram, rrpi_hist, stmin, stmax, threshold=hthresh, /nocrop, $
                   /silent, locations=locations, hist=hist
    if keyword_set(figure) then begin
      ; Adjust stretch by hand
      case prop_name[foo[j]] of
        'SOI-41-A':  stmin=0
        'SOI-42-A':  stmin=0
        'SOI-42-B':  begin
          stmin=0
          stmax=.02
        end 
        'SOI-42-C':  stmin=0
        '013-08-B':  stmax=.012
        '013-08-F':  stmax=.0103
        '013-09-B':  stmax=.0105 
        '013-10-A':  stmin=.008
        '013-10-B':  stmin=.0073
        '013-10-C':  stmin=.0073
        '013-11-B':  stmin=.008
        '013-11-C':  stmin=.008
        '013-12-G':  stmin=.0063
        '013-12-I':  stmax=.0088
        '013-12-J':  stmax=.0088
        '013-14-L':  begin
          stmin=.0055
          stmax=.0085
        end 
        '013-14-O':  stmax=.0087
        '013-14-Q':  stmax=.0087
        '013-15-E':  begin
          stmin=.006
          stmax=.008
        end 
        '013-16-A':  begin
          stmin=.006
          stmax=.008
        end 
        '031-47-B':  stmax=.0735
        '032-44-A':  stmax=.078
        '032-45-A':  stmax=.074
        '046-10-O':  stmax=.088
        '046-10-P':  stmax=.0865
        '046-11-D':  stmin=.085
        else: begin
        end 
      endcase 
    endif 
    unget_color
    tvscl, rrpi>stmin<stmax, x, y, xs=xs, ys=ys
    if keyword_set(example) then if keyword_set(doplot) then begin
      sz = size(rrpi)
      ;plots, /device, x+[0.,1,1,0,0]*xs, y+[0.,0,1,1,0]*ys, color=ctred()
      if __pp[7,foo[j]] eq 0 then begin
        ppp = __pp[*,foo[j]]
        if keyword_set(fwhm) then ppp[2:3] = ppp[2:3] * sqrt(2*alog(2))
        xxx = findgen(1000)/999*2*ppp[2]
        plots, x+( xxx + ppp[4]-ppp[2] + .5 )/sz[1]*xs, $
               y+( ppp[5] + ppp[3]* sqrt( 1 - (( xxx - ppp[2] )/$
                                               ppp[2])^2 ) + .5 )/sz[2]*ys, $
               /device, color=ctred()
        plots, x+( xxx + ppp[4]-ppp[2] + .5 )/sz[1]*xs, $
               y+( ppp[5] - ppp[3]* sqrt( 1 - (( xxx - ppp[2] )/$
                                               ppp[2])^2 ) + .5 )/sz[2]*ys, $
               /device, color=ctred()
      endif else begin
        for q=-1,1,2 do begin
          ppp = __pp[*,foo[j]]
          if keyword_set(system1) then begin
            p1 = [ ppp[0:3], ppp[4]+q*ppp[6]+q*ppp[2], ppp[5]-q*ppp[7], 0, 0 ]
          endif else begin
            p1 = [ ppp[0:3], ppp[4]+q*ppp[6], ppp[5]-q*ppp[7], 0, 0 ]
          endelse 
          if keyword_set(fwhm) then p1[2:3] = p1[2:3] * sqrt(2*alog(2))
          xxx = findgen(1000)/999*2*p1[2]
          plots, x+( xxx + p1[4]-p1[2] + .5 )/sz[1]*xs, $
                 y+( p1[5] + p1[3]*sqrt( 1 - (( xxx - p1[2] )/$
                                              p1[2])^2 ) + .5 )/sz[2]*ys, $
                 /device, color=ctred()
          plots, x+( xxx + p1[4]-p1[2] + .5 )/sz[1]*xs, $
                 y+( p1[5] - p1[3]* sqrt( 1 - (( xxx - p1[2] )/$
                                               p1[2])^2 ) + .5 )/sz[2]*ys, $
                 /device, color=ctred()
        endfor
      endelse
    endif
    if keyword_set(checkstretch) then begin
      window, 1, xs=320, ys=512, ypos=300
      !p.multi = [0,1,2]
      plot, locations, hist
      oplot, [stmin,stmin,stmax,stmax], !y.crange[[0,1,1,0]], l=1
      plot, locations, hist, yr=[0,5]
      oplot, [stmin,stmin,stmax,stmax], !y.crange[[0,1,1,0]], l=1
      window, xs=(size(rrpi))[1], ys=(size(rrpi))[2]
      !p.multi = 0
      tvscl, rrpi>stmin<stmax
      stop, stmin, stmax, '   '+prop_name[j]
    endif
    if keyword_set(multim) then _xcen = mean(xcen[multim]) else _xcen = xcen[m]
    if keyword_set(example) then begin
      pntext = prop_name[jj]
      if not keyword_set(doplot) then begin
        xyouts, /device, align=.5, _xcen, charsize=1.5, $
                !d.y_size/nrow*(nrow-rownum) - !d.y_ch_size*0, pntext
      endif
    endif else if keyword_set(figure) then begin
      pntext = prop_name[jj]
      if keyword_set(encke) or keyword_set(redge) then begin 
        pntext1 = match[jj]
      endif else begin
        pntext1 = strmid(match[jj],4,100)
      endelse 
      if keyword_set(match[jj]) then begin
        if strlen(match[jj]) gt 10 then paren1 = '!C(' else paren1 = ' ('
        if strlen(match[jj]) gt 10 then yoff = !d.y_ch_size else yoff = 0
        pntext = pntext + paren1 + pntext1 + ')'
      endif 
      xyouts, /device, align=.5, _xcen, y+ys+!d.y_ch_size*0.25+yoff, pntext
    endif else begin
      xyouts, /device, align=.5, _xcen, y+ys+!d.y_ch_size*1.5, $
              strmid(images[prop_reproj[jj].images],0,11)+$
              ', x='+strtrim(prop_reproj[jj].xy[2],2)+$
              ', y='+strtrim(prop_reproj[jj].xy[3],2)+$
              '!Crad='+string(prop_reproj[jj].radlon[4],fo='(F9.2)')+$
              ' km, lon='+string(prop_reproj[jj].radlon[5],fo='(F7.3)')+'!Uo!N'
    endelse 
  endif
endfor
j = j + 1
if keyword_set(example) then begin
  if keyword_set(doplot) then begin
    doplot = 0
  endif else begin
    j = j - ncol
    doplot = 1
    goto, nextprop
  endelse
endif
if j eq nprop then begin
  finish1:
  if not keyword_set(figure) then begin
    xyouts, /device, 0, -!d.y_ch_size, chars=1.5, text+' -- '+strtrim(k+1,2)
  endif
  if keyword_set(dolzr) then clzr
  goto, finish
;endif else if j/ncol mod nrow eq 0 then begin
endif else if rownum eq nrow-1 then begin
  finishpage:
  if not keyword_set(figure) then begin
    xyouts, /device, 0, -!d.y_ch_size, chars=1.5, text+' -- '+strtrim(k+1,2)
  endif 
  k = k + 1
  if keyword_set(dolzr) then begin
    if keyword_set(onepage) then clzr else begin
      plot_blank
      goto, nextpage2
    endelse 
  endif else stop
  goto, nextpage1
endif
goto, nextprop

finish:

end
