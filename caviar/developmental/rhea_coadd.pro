restore, 'stretch.sav'
if keyword_set(findfile('rheaxy.sav')) then restore, 'rheaxy.sav' else begin
  rheaxy = lonarr( 2, n_elements(filenames) )
  for jjj=0,n_elements(filenames)-1 do begin
    image_name=filenames[jjj]
    @caviar
    rheaxy[*,jjj] = [ xp[5], yp[5] ]
  endfor
  save, rheaxy, filename='rheaxy.sav'
endelse

if not keyword_set(minim) then minim = 0
if not keyword_set(maxim) then maxim = n_elements(filenames)-1
meanim = fix(mean([ minim, maxim ]))
savefile = 'add'+strtrim(minim,2)+'thru'+strtrim(maxim,2)+'.sav'
if keyword_set(findfile(savefile)) then restore, savefile else begin
  print, 'Co-adding images '+strtrim(minim,2)+' thru '+strtrim(maxim,2)+'...'
  if not keyword_set(nl) then nl = 1024
  sumim = fltarr( nl, nl )
  xshmn = 0
  xshmx = 0
  yshmn = 0
  yshmx = 0
  for jjj=minim,maxim do begin
    _im = read_vicar( filenames[jjj] )
    xsh = rheaxy[0,meanim] - rheaxy[0,jjj]
    ysh = rheaxy[1,meanim] - rheaxy[1,jjj]
    sumim = sumim + shift( _im, xsh, ysh )
    xshmn = xshmn < xsh
    xshmx = xshmx > xsh
    yshmn = yshmn < ysh
    yshmx = yshmx > ysh
  endfor
  ;if xshmx gt 0 then sumim[0:xshmx-1,*] = 0
  ;if xshmn lt 0 then sumim[nl+xshmn:nl-1,*] = 0
  ;if yshmx gt 0 then sumim[*,0:yshmx-1] = 0
  ;if yshmn lt 0 then sumim[*,nl+yshmn:nl-1] = 0
  sumim[0:xshmx,*] = rebin( sumim[xshmx+1,*], xshmx+1, nl )
  sumim[nl+xshmn-1:nl-1,*] = rebin( sumim[nl+xshmn-2,*], -xshmn+1, nl )
  sumim[*,0:yshmx] = rebin( sumim[*,yshmx+1], nl, yshmx+1 )
  sumim[*,nl+yshmn-1:nl-1] = rebin( sumim[*,nl+yshmn-2], -yshmn+1, nl )
  save, sumim, xshmn, xshmx, yshmn, yshmx, filename=savefile
endelse

image_name=filenames[meanim]
@caviar
if keyword_set(hipass) then begin
  if not keyword_set(sm) then sm = 15
  rawim = sumim - smooth( sumim, sm, /edge_truncate )
endif else begin
  rawim = sumim
endelse
@disp_rawim

end
