dir = [ '046/RDHRESSCN', '071/RDHRSSCHP', '077/RDHRCOMP', '132/PROPELLR' ]
ndir = n_elements(dir)
xypos = [ [120,0.82], [80,0.9], [125.2,0.97], [130,0.58] ]
xyalign = [ 1, 0, 0, 1 ]
savefile = 'ringsres_radial_scales.sav'
; Next step is to make an array of radius values at 1-km resolution,
; start with all values set to 2, then reduce them whenever covered by
; one of the radial scans below. 
;maxres = 
if keyword_set(findfile(savefile)) then restore, savefile else begin
  for j=0l,ndir-1 do begin
    restore, '$DATA/images/' + dir[j] + '/stretch.sav'
    restore, '$DATA/images/' + dir[j] + '/max_radi.sav'
    if j eq 0 then begin
      _dir = replicate(dir[j],nfiles)
      _filenames = filenames[foo]
      _min_radi = min_radi
      _max_radi = max_radi 
      _radscale = reform(_keywords[foo].ringplane_aimpoint_radial_scale)
      _nfiles = nfiles
    endif else begin
      _dir = [ _dir, replicate(dir[j],nfiles) ]
      _filenames = [ _filenames, filenames[foo] ]
      _min_radi = [ _min_radi, min_radi ]
      _max_radi = [ _max_radi, max_radi ]
      _radscale = [ _radscale, $
                    reform(_keywords[foo].ringplane_aimpoint_radial_scale) ]
      _nfiles = _nfiles + nfiles
    endelse 
  endfor
endelse

minrad = 74490.0d0
maxrad = 136774.0d0
nradi = maxrad-minrad+1
radi = dindgen(nradi) + minrad
maxscale = fltarr(nradi)
maxscale[*] = 2
for k=0,_nfiles-1 do begin
  foo = where( radi ge _min_radi[k] and radi le _max_radi[k], count )
  if count gt 0 then begin
    maxscale[foo] = maxscale[foo] < _radscale[k]
  endif 
endfor

today = current_time_6digit()
if keyword_set(dolzr) then begin
  lzr, 'ringsres_radial_scales_'+today
  @plot_prepare
  plot_color
endif
!p.charsize = 1.5
device, decompose=0
plot, [0], [0], /nodata, xtit='Radius'+tkmtit(), ytit='Radial Scale (km/px)', $
      xr=[70,142], xs=5, yr=[0.5,1.5], /ys
polyfill, [tkm(radi),reverse(tkm(radi))], $
          [maxscale,replicate(0,nradi)], co=ltgray(), noclip=0
axis, xaxis=1, xr=!x.crange, /xs, xtickn = replicate(' ',20)
axis, xaxis=0, xr=!x.crange, /xs, xtit='Radius'+tkmtit()
for k=0l,_nfiles-1 do begin
  oplot, tkm([_min_radi[k],_max_radi[k]]), _radscale[[k,k]]
endfor 
for j=0l,ndir-1 do xyouts, xypos[0,j], xypos[1,j], dir[j], align=xyalign[j]
xyouts, 83, 1.23, '046/RDHRESSCN'
xyouts, 105, 1.4, '077/RDHRCOMP'
n1 = where( dir eq '132/PROPELLR' )
n2 = where( _dir eq '132/PROPELLR' )
for j=0,1 do arrow, xypos[0,n1]+0.5, xypos[1,n1]+0.01, $
                    tkm(_min_radi[n2[j]])-0.3, _radscale[n2[j]], $
                    hsize=!d.x_size/128, hthick=1, /solid, /data
rads = [ 70, 74.658, 91.975, 117.507, 122.34, 136.78, 140.219 ]
ints1 = rebin( [ [rads[1:5]], [rads[0:4]] ], 5, 1 )
ints1 = [ ints1, rads[6] ]
intnames = [ 'D', 'C', 'B', 'CD', 'A', 'F' ]
for j=0,n_elements(rads)-1 do oplot, rads[j]*[1,1], [-1e10,1e10], l=1
xyouts, ints1, replicate(.52,n_elements(ints1)), intnames, align=.5, chars=1
if keyword_set(dolzr) then clzr

end
