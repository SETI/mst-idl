; Compare identified propeller-candidate locations from adjacent images, 
; look for any that appear in multiple images
sfile = 'image_outlines.sav'
outlinex = [ indgen(1023), replicate(1023,1023), $
             reverse(indgen(1023)+1), replicate(0,1023) ]
outliney = [ replicate(0,1023), indgen(1023), $
             replicate(1023,1023), reverse(indgen(1023)+1) ]
nol = n_elements(outlinex)
if not keyword_set(findfile(sfile)) then begin
  restore, 'stretch.sav'
  restore, 'et.sav'
  filenames = filenames[where( strmid(filenames,0,1) eq 'N' )]
  nf = n_elements(filenames)
  dot = rstrpos( filenames, '.' )
  filestem = strarr(nf)
  for jjj=0,nf-1 do filestem[jjj] = strmid( filenames[jjj], 0, dot[jjj] )
  @get_cam_params
  cspice_furnsh, getenv("CAVIAR_KERNELS")
  for jjj=0,nf-1 do begin
    offsetfile = filestem[jjj] + '.offset'
    print, strtrim(jjj,2) + ' / ' + strtrim(nf,2) + '     ' + filestem[jjj]
    if keyword_set(findfile(offsetfile)) then restore, offsetfile else begin
      openr, 1, strmid( filestem[jjj], 0, strlen(filestem[jjj])-4 ) + '.LBL'
      aa = ''
      label = [ [['EXPOSURE_DURATION',''], ['INSTRUMENT_ID',''], $
                 ['SPACECRAFT_CLOCK_STOP_COUNT',''], ['NL','']] ]
      readf, 1, aa
      while strmid(aa,0,17) ne label[0,0] do readf, 1, aa
      equal = strpos(aa,'=')
      label[1,0] = strmid(aa,equal+1,1000)
      while strmid(aa,0,13) ne label[0,1] do readf, 1, aa
      equal = strpos(aa,'=')
      label[1,1] = strmid(aa,equal+1,1000)
      while strmid(aa,0,27) ne label[0,2] do readf, 1, aa
      equal = strpos(aa,'=')
      label[1,2] = strmid(aa,equal+1,1000)
      while strmid(aa,0,11) ne '      LINES' do readf, 1, aa
      equal = strpos(aa,'=')
      label[1,3] = strmid(aa,equal+1,1000)
      close, 1
      for j=0,3 do begin
        quotes1 = strpos( label[1,j], '"' )
        if quotes1 ne -1 then begin
          quotes2 = rstrpos( label[1,j], '"' )
          label[1,j] = strmid( label[1,j], quotes1+1, quotes2-quotes1-1 )
        endif 
      endfor 
      image_data, label, _et[jjj], epoch, exposure, cam_name, pmat, nl, found
      cmat = nacmat ## pmat
    endelse 
    p2radec, cam_params, cmat, nl, outliney, outlinex, cra, cdec
    p2ralon, cmat, _et[jjj], polera, poledec, sc, cra, cdec, outliner, outlinel
    dlcheck = max(outlinel) - min(outlinel)
    if dlcheck gt 300 then begin
      foo = where( outlinel lt 180 )
      outlinel[foo] = outlinel[foo] + 360
    endif
    if jjj eq 0 then begin
      outline_rad = rotate(outliner,1)
      outline_lon = rotate(outlinel,1)
    endif else begin
      outline_rad = [ outline_rad, rotate(outliner,1) ]
      outline_lon = [ outline_lon, rotate(outlinel,1) ]
    endelse
  endfor
  image_num = long(strmid( filenames, 1, 10 ))
  image_nums = image_num[ uniq(image_num) ]
  spawn, 'pwd', pwd
  lslashes = rstrpos( pwd, '/' )
  lslashes = [ lslashes, rstrpos( pwd, '/', lslashes ) ]
  rev = strmid( pwd, lslashes[1]+1, lslashes[0]-lslashes[1]-1 )
  obsname = strlowcase(strmid( pwd, lslashes[0]+1, 1000 ))
  save, image_num, image_nums, outline_rad, outline_lon, nf, rev, obsname, $
        nol, filename=sfile
endif else restore, sfile
if nf eq 1 then image_nums = reform(image_nums,1,1)
outline_lon_corr = outline_lon - sqrt(caviar_omega2(outline_rad)) * 180/!dpi * $
                   ( rebin(image_nums,nf,nol) - image_num[nf/2] )
if keyword_set(decrease) then begin
  lons = [ max(outline_lon_corr), min(outline_lon_corr) ]
endif else begin
  lons = [ min(outline_lon_corr), max(outline_lon_corr) ]
endelse
if keyword_set(dolzr) then begin
  if keyword_set(doubleplot) then if not keyword_set(qport) then port=1
  if keyword_set(qport) or keyword_set(port) then half = 0
  if not keyword_exists(half) then half = 1
  lzr, 'image_outlines_' + rev + obsname, port=port, qport=qport, half=half
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
if keyword_set(_yr) then yr = _yr else begin
  yr = tkm([ min(outline_rad), max(outline_rad) ])
endelse
if keyword_set(_xr) then xr = _xr else xr = lons
if not keyword_set(tit) then tit = ''
plot, /nodata, xr, yr, /xs, /ys, tit=tit, $
      xtit='Co-Rotating Longitude (!Uo!N)', ytit='Radius'+tkmtit()
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
  polyfill, outline_lon_corr[j,*], tkm(outline_rad[j,*]), co=clr[j mod nclr]
endfor

if keyword_set(doubleplot) then if reverse eq 0 then goto, redo
if keyword_set(dolzr) then clzr

end
