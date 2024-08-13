pro diffring_exposures, run, filenames, data, level, dir, phase, emission, $
  inbox, immax, verbose=verbose, old=old

dirbase = '/home/borogove/iss/images/'
if keyword_set(old) then old = 'old/' else old = ''
case run of 
  1: begin  ;017_G60PHASE
    _x0=275 & _x1=325 & _y0=250 & _y1=260
    _x2=195 & _x3=245 & _y2=250 & _y3=260
    dir = '017/G60PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'017_g60phase.sav'
  end
  2: begin  ;017_E60PHASE
    _x0=240 & _x1=270 & _y0=215 & _y1=225
    _x2=240 & _x3=270 & _y2=295 & _y3=305
    dir = '017/E60PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'017_e60phase.sav'
  end
  3: begin  ;018_G105PHASE
    ;_x0=275 & _x1=325 & _y0=255 & _y1=265
    _x0=335 & _x1=395 & _y0=255 & _y1=265
    dir = '018/G105PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'018_g105phase.sav'
  end
  4: begin  ;018_E105PHASE
    _x0=250 & _x1=270 & _y0=215 & _y1=225
    _x2=250 & _x3=270 & _y2=315 & _y3=325
    dir = '018/E105PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'018_e105phase.sav'
  end
  5: begin  ;019_E105PHASE
    _x0=250 & _x1=270 & _y0=215 & _y1=225
    _x2=250 & _x3=270 & _y2=315 & _y3=325
    dir = '019/E105PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'019_e105phase.sav'
  end
  6: begin  ;019_G105PHASE
    _x0=310 & _x1=315 & _y0=257 & _y1=258
    dir = '019/G105PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'019_g105phase.sav'
  end
  7: begin  ;020_E80PHASE
    _x0=250 & _x1=260 & _y0=250 & _y1=260
    _x2=355 & _x3=365 & _y2=250 & _y3=260
    dir = '020/E80PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'020_e80phase.sav'
  end
  8: begin  ;020_E135PHASE
    _x0=210 & _x1=220 & _y0=205 & _y1=215
    dir = '020/E135PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'020_e135phase.sav'
  end
  9: begin  ;021_G80PHASE
    _x0=315 & _x1=320 & _y0=237 & _y1=241
    dir = '021/G80PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'021_g80phase.sav'
  end
  10: begin  ;021_E135PHASE
    _x0=250 & _x1=280 & _y0=250 & _y1=270
    _x2=250 & _x3=280 & _y2=350 & _y3=370
    dir = '021/E135PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'021_e135phase.sav'
  end
  11: begin  ;020_RETHIEQPL
    _x0=290 & _x1=300 & _y0=275 & _y1=300
    ims = [ 0, 1, 18, 19, 20, 21, 34, 35 ]
    dir = '020/RETHIEQPL/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'020_rethieqpl.sav'
  end
  12: begin  ;019_RETHIEQPL
    _x0=410 & _x1=425 & _y0=70 & _y1=85
    ims = [ 4, 6, 8, 10 ]
    dir = '019/RETHIEQPL/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'019_rethieqpl.sav'
  end
  13: begin  ;017/E105PHASE
    _x0=250 & _x1=270 & _y0=215 & _y1=225
    _x2=250 & _x3=270 & _y2=315 & _y3=325
    dir = '017/E105PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'017_e105phase.sav'
  end
  14: begin  ;022_E105PHASE
    _x0=250 & _x1=270 & _y0=215 & _y1=225
    _x2=250 & _x3=270 & _y2=315 & _y3=325
    dir = '022/E105PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'022_e105phase.sav'
  end
  15: begin  ;022_RETHIEQPL005
    _x0=290 & _x1=300 & _y0=275 & _y1=300
    ims = [ 0, 1, 3, 21, 23, 25 ]
    dir = '022/RETHIEQPL005/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'022_rethieqpl005.sav'
  end
  16: begin  ;023_E105PHASE
    _x0=250 & _x1=270 & _y0=215 & _y1=225
    _x2=250 & _x3=270 & _y2=315 & _y3=325
    dir = '023/E105PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'023_e105phase.sav'
  end
  17: begin  ;025_G150PHASE
    _x0=205 & _x1=225 & _y0=115 & _y1=135
    _x2=165 & _x3=185 & _y2=115 & _y3=135
    dir = '025/G150PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'025_g150phase.sav'
  end
  18: begin  ;025_E150PHASE
    _x0=255 & _x1=275 & _y0=205 & _y1=225
    _x2=255 & _x3=275 & _y2=5 & _y3=25
    dir = '025/E150PHASE/'
    savefile = '~/idl/iss/diffring_exposures/'+old+'025_e150phase.sav'
  end
  else: stop, 'Run '+strtrim(run,2)+' not recognized'
endcase

if keyword_set(findfile(savefile)) then begin
  restore, savefile
  nf = n_elements(filenames)
endif else begin

  if not keyword_set(filenames) then restore, dirbase+dir+'stretch.sav'
  if not keyword_set(data) then restore, dirbase+dir+'spreadsheet.sav'
  nf = n_elements(filenames)
  if keyword_set(ims) then begin
    filenames = filenames[ims]
    nf = n_elements(filenames)
  endif else begin
    ims = indgen(nf)
  endelse
  phase = _keywords[ims].ringplane_aimpoint_phase_angle
  emission = _keywords[ims].ringplane_aimpoint_emission_angle

  index = intarr(nf)
  maxpx = lonarr(nf)
  inbox = intarr(nf)
  cal = strpos( filenames, '_cal' )
  level = fltarr( nf, 8 )
  _thresh = 10
  if keyword_set(findfile(dirbase+dir+'immax.sav')) then begin
    restore, dirbase + dir + 'immax.sav'
    immax_restored = 1
  endif else begin
    immax = lonarr(2,nf)
  endelse

  for j=0,nf-1 do begin
    foo = where( data[0,*] eq strmid(filenames[j],0,cal[j]), count )
    if count ne 1 then stop, 'Number of filenames ne 1'
    index[j] = foo
  endfor
  data = data[*,index]

  for j=0,nf-1 do begin

    if data[7,j] eq 'SUM2' then begin
      x = 1
      thresh = _thresh 
      x0=_x0 & x1=_x1 & y0=_y0 & y1=_y1
      if keyword_set(_x2) then begin
        x2=_x2 & x3=_x3 & y2=_y2 & y3=_y3
      endif else begin
        x2=_x0 & x3=_x1 & y2=_y0 & y3=_y1
      endelse
    endif else if data[7,j] eq 'FULL' then begin
      x = 2
      thresh = 4*_thresh
      x0=2*_x0 & x1=2*_x1 & y0=2*_y0 & y1=2*_y1
      if keyword_set(_x2) then begin
        x2=2*_x2 & x3=2*_x3 & y2=2*_y2 & y3=2*_y3
      endif else begin
        x2=2*_x0 & x3=2*_x1 & y2=2*_y0 & y3=2*_y1
      endelse
    endif else stop, data[7,j]

    im = read_vicar( dirbase + dir + filenames[j] )
    nl = (size(im))[1]
    if type(im) eq 2 then im = float(im)
    run_histogram, im, stmin, stmax, locations=locations, hist=hist, $
                   threshold=thresh

    xx = 3
    if keyword_set(immax_restored) then begin
      _immax = immax[*,j]
    endif else begin
      _immax = wher( smooth(im,25,/edge) eq $
               max((smooth(im,25,/edge))[100*x:nl-100*x,100*x:nl-100*x]), c )
      foo = where( ( _immax[0,*] ge x0 and _immax[0,*] le x1 and $
                     _immax[1,*] ge y0 and _immax[1,*] le y1 ) or $
                   ( _immax[0,*] ge x2 and _immax[0,*] le x3 and $
                     _immax[1,*] ge y2 and _immax[1,*] le y3 ), count )
      if count ge 1 then begin
        if c gt 1 then _immax = _immax[*,foo[0]]
        inbox[j] = 1
      endif else begin
        if c gt 1 then _immax = _immax[*,0];stop, _immax
        inbox[j] = 0
      endelse
      _immax = _immax > (3+xx*x) < (nl-4-xx*x)
    endelse

    level[j,4] = (locations[where( hist eq max(hist) )])[0]
    if _immax[0] eq -1 then level[j,5] = -1 else begin
      level[j,5] = mean(im[_immax[0]-xx*x:_immax[0]+xx*x,$
                             _immax[1]-xx*x:_immax[1]+xx*x])
    endelse
    level[j,6] = mean(im)
    level[j,7] = mean(im[x0:x1,y0:y1]) > mean(im[x2:x3,y2:y3])

    im = read_vicar( dirbase + dir + strmid(filenames[j],0,cal[j]) + '.IMG' )
    if type(im) eq 2 then im = float(im)
    run_histogram, im, stmin, stmax, locations=locations, hist=hist, $
                   threshold=thresh

    level[j,0] = (locations[where( hist eq max(hist) )])[0]
    if _immax[0] eq -1 then level[j,1] = -1 else begin
      level[j,1] = mean(im[_immax[0]-xx*x:_immax[0]+xx*x,$
                             _immax[1]-xx*x:_immax[1]+xx*x])
    endelse
    level[j,2] = mean(im)
    level[j,3] = mean(im[x0:x1,y0:y1]) > mean(im[x2:x3,y2:y3])
    maxpx[j] = max(im[3:nl-4,3:nl-4])
    immax[*,j] = _immax

  endfor

  if run eq 1 then begin
    ; Remove an image that is part-missing, leading to 
    ; too small of a background (zeroes got averaged in)
    level[42,[1,5]] = -1
  endif

  save, filenames, data, level, phase, emission, inbox, immax, ims, $
        filename=savefile

endelse

if not keyword_exists(verbose) then verbose = 1
if keyword_set(verbose) then for j=0,nf-1 do begin
  print, data[4,j]+'  '+data[5,j]+'  '+data[3,j]+'  '+strmid(data[6,j],0,3)+$
         '  '+data[7,j]+'  '+strtrim(level[j,0],2)+$
         '  '+strtrim(level[j,1],2)+'  '+strtrim(level[j,2],2)+$
         '  '+strtrim(level[j,3],2)+'  '+strtrim(level[j,4],2)+$
         '  '+strtrim(level[j,5],2)+'  '+strtrim(level[j,6],2)+$
         '  '+strtrim(level[j,7],2)+'  '+data[0,j]
endfor

end

