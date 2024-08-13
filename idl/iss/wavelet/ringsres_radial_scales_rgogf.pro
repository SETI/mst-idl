dir_lit = [ '256/HRRADSCNL', '268/HRRADSCNL', '277/HRRADSCNL', '283/HRRADSCNL' ]
dir_unlit = [ '268/HRRADSCNU', '276/HRRADSCNU', '283/HRRADSCNU' ]
if not keyword_set(unlit) then lit = 1
if keyword_set(lit) and keyword_set(unlit) then stop, 'Lit or unlit?'
if keyword_set(lit) then dir = dir_lit else dir = dir_unlit
if keyword_set(suprhres) then begin
  dir = [ dir, '253/SUPRHRESU', '263/SUPRHRESL', '270/SUPRHRESL', $
          '270/SUPRHRESU' ]
endif
if keyword_set(propclos) then begin
  dir = [ dir, '257/DAPHNIS', '262/PROPCLOSL', '262/PROPCLOSU', '264/PAN', $
          '266/PROPCLOSL', '268/PROPCLOSU' ]
endif
ndir = n_elements(dir)
xypos = [ [120,0.82], [80,0.9], [125.2,0.97], [130,0.58] ]
xyalign = [ 1, 0, 0, 1 ]
savefile = 'ringsres_radial_scales_'
if not keyword_set(lit) then savefile = savefile + 'un'
savefile = savefile + 'lit.sav'
; Next step is to make an array of radius values at 1-km resolution,
; start with all values set to 2, then reduce them whenever covered by
; one of the radial scans below. 
;maxres = 
if keyword_set(findfile(savefile)) then restore, savefile else begin
  for j=0l,ndir-1 do begin
    restore, '$DATA/images/' + dir[j] + '/stretch.sav'
    restore, '$DATA/images/' + dir[j] + '/max_radi.sav'
    if dir[j] eq '276/HRRADSCNU' and not keyword_set(suprhres) then begin
      foo = foo[10:nfiles-1]
      max_radi = max_radi[foo]
      min_radi = min_radi[foo]
      nfiles = n_elements(foo)
    endif 
    if dir[j] eq '277/HRRADSCNL' and not keyword_set(suprhres) then begin
      foo = foo[0:41]
      max_radi = max_radi[foo]
      min_radi = min_radi[foo]
      nfiles = n_elements(foo)
    endif 
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
  psfile = 'ringsres_radial_scales_'
  if not keyword_set(lit) then psfile = psfile + 'un'
  psfile = psfile + 'lit_'+today
  lzr, psfile
  @plot_prepare
  plot_color
endif
!p.charsize = 1.5
device, decompose=0
plot, [0], [0], /nodata, xtit='Radius'+tkmtit(), ytit='Radial Scale (km/px)', $
      xr=[70,142], xs=5, yr=[0.3,1.25], /ys
polyfill, [tkm(radi),reverse(tkm(radi))], $
          [maxscale,replicate(0,nradi)], co=ltgray(), noclip=0
axis, xaxis=1, xr=!x.crange, /xs, xtickn = replicate(' ',20)
axis, xaxis=0, xr=!x.crange, /xs, xtit='Radius'+tkmtit()
clr = [ red(), blue(), green(), purple(), cyan() ]
clr = [ clr, clr ]
for k=0l,_nfiles-1 do begin
  if keyword_set(plotcolor) then begin
    m = (where( _dir[k] eq dir ))[0]
    oplot, tkm([_min_radi[k],_max_radi[k]]), _radscale[[k,k]], $
           co=clr[m]
  endif else begin
    oplot, tkm([_min_radi[k],_max_radi[k]]), _radscale[[k,k]]
  endelse
endfor 
for m=0,n_elements(dir)-1 do begin
  xyouts, align=1, 138, 1.15 - m*0.07, dir[m], co=clr[m]
endfor
;for j=0l,ndir-1 do xyouts, xypos[0,j], xypos[1,j], dir[j], align=xyalign[j]
;xyouts, 83, 1.23, '046/RDHRESSCN'
;xyouts, 105, 1.4, '077/RDHRCOMP'
;n1 = where( dir eq '132/PROPELLR' )
;n2 = where( _dir eq '132/PROPELLR' )
;for j=0,1 do arrow, xypos[0,n1]+0.5, xypos[1,n1]+0.01, $
;                    tkm(_min_radi[n2[j]])-0.3, _radscale[n2[j]], $
;                    hsize=!d.x_size/128, hthick=1, /solid, /data
if keyword_set(dolzr) then clzr

stop, 'Continue for comparison of RGO/GF to ringsres'

radi_rgogf = radi
maxscale_rgogf = maxscale
savefile2 = 'maxscale_rgogf_'
if not keyword_set(lit) then savefile2 = savefile2 + 'un'
savefile2 = savefile2 + 'lit.sav'
if not keyword_set(findfile(savefile2)) then begin
  save, radi_rgogf, maxscale_rgogf, filename=savefile2
endif

restore, 'maxscale.sav'
plot, tkm(radi), maxscale/maxscale_rgogf, xr=[70,142], yr=[0.7,2], /xs, /ys, $
      xtit='Radius'+tkmtit(), $
      ytit='Best pixel scale, pre-RGO/GF divided by RGO/GF'

end
