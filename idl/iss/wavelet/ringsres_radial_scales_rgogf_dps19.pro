if keyword_set(plotim) then goto, justplots
dir_lit = [ '256/HRRADSCNL', '268/HRRADSCNL', '277/HRRADSCNL', '283/HRRADSCNL' ]
dir_unlit = [ '268/HRRADSCNU', '276/HRRADSCNU', '283/HRRADSCNU' ]
if not keyword_set(unlit) then lit = 1
if keyword_set(lit) and keyword_set(unlit) then stop, 'Lit or unlit?'
if keyword_set(lit) then dir = dir_lit else dir = dir_unlit
if not keyword_exists(suprhres) then suprhres = 1
if not keyword_exists(plotcolor) then plotcolor = 1
if keyword_set(suprhres) then begin
  ;dir = [ dir, '253/SUPRHRESU', '263/SUPRHRESL', '270/SUPRHRESL', $
  ;        '270/SUPRHRESU' ]
  if keyword_set(lit) then begin
    dir = [ dir, '263/SUPRHRESL', '270/SUPRHRESL' ]
  endif else begin
    dir = [ dir, '253/SUPRHRESU', '270/SUPRHRESU' ]
  endelse
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
bestimage = strarr(nradi)
bestimagedir = strarr(nradi)
for k=0,_nfiles-1 do begin
  foo = where( radi ge _min_radi[k] and radi le _max_radi[k], count )
  if count gt 0 then begin
    ;maxscale[foo] = maxscale[foo] < _radscale[k]
    foo2 = where( _radscale[k] lt maxscale[foo], count )
    if count gt 0 then begin
      maxscale[foo[foo2]] = _radscale[k]
      bestimage[foo[foo2]] = _filenames[k]
      bestimagedir[foo[foo2]] = _dir[k]
    endif 
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
clr = vec_remove( clr, 7 )
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

;stop, 'Continue for comparison of RGO/GF to ringsres'
;
;radi_rgogf = radi
;maxscale_rgogf = maxscale
;savefile2 = 'maxscale_rgogf_'
;if not keyword_set(lit) then savefile2 = savefile2 + 'un'
;savefile2 = savefile2 + 'lit.sav'
;if not keyword_set(findfile(savefile2)) then begin
;  save, radi_rgogf, maxscale_rgogf, filename=savefile2
;endif
;
;restore, 'maxscale.sav'
;plot, tkm(radi), maxscale/maxscale_rgogf, xr=[70,142], yr=[0.7,2], /xs, /ys, $
;      xtit='Radius'+tkmtit(), $
;      ytit='Best pixel scale, pre-RGO/GF divided by RGO/GF'

radidps = radi
bestimagesdir = bestimagedir[uniq( bestimage, sort(bestimage) )]
bestimages = bestimage[uniq( bestimage, sort(bestimage) )]
nbi = n_elements(bestimages)
drdps = 0.1d0
dldps = 0.1d0
drnum = fix(1.0d0/drdps)
if abs( drnum - 1.0d0/drdps ) gt 1e-8 then stop, $
                             'drdps should be the reciprocal of an integer.'
lwidth = 10.0d0
rx = lwidth / dldps
radidps2 = radidps
bestimage2 = bestimage
bestimagedir2 = bestimagedir
for j=1,drnum-1 do radidps2 = [ radidps2, radidps+j*drdps ]
for j=1,drnum-1 do bestimage2 = [ bestimage2, bestimage ]
for j=1,drnum-1 do bestimagedir2 = [ bestimagedir2, bestimagedir ]
bestimage2 = bestimage2[sort(radidps2)]
bestimagedir2 = bestimagedir2[sort(radidps2)]
radidps2 = radidps2[sort(radidps2)]
nradi2 = n_elements(radidps2)
bigmapfile = 'bigmap_dps19.sav'
if keyword_set(findfile(bigmapfile)) then restore, bigmapfile else begin
  bigmap = fltarr(rx,nradi/drdps)
  bigmap_bksub = fltarr(rx,nradi/drdps)
  for i=0,nbi-1 do begin
    jjj = (where( _filenames eq bestimages[i] ))[0]
    bifoo = where( bestimage eq bestimages[i], nbifoo )
    birmin = radidps[min(bifoo)]
    birmax = radidps[max(bifoo)] + 1 - drdps
    noplot = 1
    image_name = '/home/sauron2/iss/images/'+bestimagesdir[i]+'/'+bestimages[i]
    bksub = 1
    dps19map_repeat:
    bksub = 1 - bksub
    @caviar
    print, ''
    print, '--------------'
    print, strtrim(i,2)+' / '+strtrim(nbi,2)+':  '+$
           bestimagesdir[i]+'/'+strmid(bestimages[i],0,11)
    print, '--------------'
    print, ''
    get_radarray, cam_params, cmat, nl, et, polera, poledec, sc, $
                  radarray, lonarray
    rcorr = mean([ _min_radi[jjj] - min(radarray), $
                   _max_radi[jjj] - max(radarray) ])
    mnrad = birmin - rcorr
    mxrad = birmax - rcorr
    mnlon = mean([min(lonarray),max(lonarray)]) - 10.0d0/mean(radarray)*180/!dpi
    mxlon = mean([min(lonarray),max(lonarray)]) + 10.0d0/mean(radarray)*180/!dpi
    forcexy = 1
    ry = (birmax-birmin) / drdps + 1
    @r
    bifoo2 = bifoo/drdps
    for j=1,drnum-1 do bifoo2 = [bifoo2,bifoo2+j]
    bifoo2 = bifoo2[sort(bifoo2)]
    if keyword_set(bksub) then begin
      bigmap_bksub[*,bifoo2] = rrpi[*,bifoo2-bifoo2[0]]
    endif else begin
      bigmap[*,bifoo2] = rrpi[*,bifoo2-bifoo2[0]]
    endelse 
    if not keyword_set(bksub) then goto, dps19map_repeat
    ;if i mod 10 eq 0 and i ne 0 then stop
  endfor 

  ; Adjust unlit-side images to higher brightness, so that they fit. 
  foo253 = where( bestimagedir2 eq '253/SUPRHRESU' )
  foo253a = where( bestimagedir2 eq '253/SUPRHRESU' and radidps2 lt 120000 )
  bigmap[*,foo253a] = bigmap[*,foo253a] * 15
  foo253b = where( bestimagedir2 eq '253/SUPRHRESU' and $
                   radidps2 gt 120000 and radidps2 lt 133000 )
  bigmap[*,foo253b] = bigmap[*,foo253b] * 8
  foo253c = where( bestimagedir2 eq '253/SUPRHRESU' and $
                   radidps2 gt 133000 and radidps2 lt 136000 )
  bigmap[*,foo253c] = bigmap[*,foo253c] * 5
  foo253d = where( bestimagedir2 eq '253/SUPRHRESU' and $
                   radidps2 gt 136000 )
  bigmap[*,foo253d] = bigmap[*,foo253d] * 3
  foo270 = where( bestimagedir2 eq '270/SUPRHRESU' )
  bigmap[*,foo270] = bigmap[*,foo270] * 15

  foo270lb = where( bestimagedir2 eq '270/SUPRHRESL' and radidps2 lt 115000 )
  bigmap[*,foo270lb] = bigmap[*,foo270lb] * 2 / 2.8
  foo256cenb = where( bestimagedir2 eq '256/HRRADSCNL' and radidps2 lt 110300 )
  bigmap[*,foo256cenb] = bigmap[*,foo256cenb] * 2 / 2.5

  cring = where( radidps2 lt 92000 )
  bigmap[*,cring] = bigmap[*,cring] / 2 + 0.05
  cassdiv = where( radidps2 gt 117580 and radidps2 lt 122100 )
  bigmap[*,cassdiv] = bigmap[*,cassdiv] / 2 + 0.05

  if keyword_set(diagnostic_plots) then begin
    plot, tkm(radidps2), bigmap[40,*], ps=3, /xs, yr=[-.1,.5]
    if keyword_set(byobs) then begin
      oplot, tkm(radidps2[foo253]), bigmap[40,foo253], ps=3, co=blue()
      oplot, tkm(radidps2[foo270]), bigmap[40,foo270], ps=3, co=red()
      oplot, tkm(radidps2[foo270lb]), bigmap[40,foo270lb], ps=3, co=green()
      oplot, tkm(radidps2[foo256cenb]), bigmap[40,foo256cenb], ps=3, co=yellow()
    endif else if keyword_set(byregion) then begin
      oplot, tkm(radidps2[cring]), bigmap[40,cring], ps=3, co=blue()
      oplot, tkm(radidps2[cassdiv]), bigmap[40,cassdiv], ps=3, co=red()
    endif
  endif
  save, bigmap, bigmap_bksub, filename=bigmapfile
endelse

minioverf = 0.05
maxioverf = 0.23
bksubioverf = 0.0002
bigmapim = ( bigmap>minioverf<maxioverf - minioverf )
bigmapim = bigmapim/( maxioverf - minioverf )*255
bigmapim_bksub = bigmap_bksub>(-bksubioverf)<bksubioverf + bksubioverf
bigmapim_bksub = bigmapim_bksub/(2*bksubioverf)*255

textures2 = strarr(nradi2)
for i=0,nbi-1 do begin
  jjj = (where( _filenames eq bestimages[i] ))[0]
  bifoo = where( bestimage2 eq bestimages[i], nbifoo )
  birmin = radidps2[min(bifoo)]
  birmax = radidps2[max(bifoo)] + 1 - drdps
  lastdot = rstrpos( bestimages[i], '.' )
  filestem = strmid( bestimages[i], 0, lastdot )
  filetype = [ 'classification_array.txt', 'clicker_radius.txt' ]
  dontuse = 0
  ntex = 0
  for q=0,1 do begin
    openfile = '/home/sauron2/iss/images/jmodesto/'+bestimagesdir[i]+'/'+$
               filestem+'_'+filetype[q]
    if keyword_set(findfile(openfile)) then openr, 1, openfile else begin
      print, 'Missing '+openfile
      dontuse = 1
      goto, nextfile
    endelse 
    aa = ''
    texturearray = ''
    while not eof(1) do begin
      readf, 1, aa
      if not keyword_set(texturearray) then texturearray = aa else begin
        texturearray = [ texturearray, aa ]
      endelse 
    endwhile
    close, 1
    case q of
      0: begin
        textures = texturearray
        ntex = n_elements(textures)
      end
      1: begin
        texturerads = texturearray[sort(texturearray)]
        if n_elements(texturerads) ne ntex+1 then begin
          print, 'Check texture array: '+openfile
          dontuse = 1
        endif
      end
    endcase 
    nextfile:
  endfor
  if not keyword_set(dontuse) then begin
    for itex=0,ntex-1 do begin
      foo = where( radidps2[bifoo] gt texturerads[itex] and $
                   radidps2[bifoo] lt texturerads[itex+1], count )
      textures2[bifoo[foo]] = textures[itex]
    endfor
;    !p.multi = [0,1,2]
;    imdisp, rotate(bigmapim_bksub[*,bifoo[0]:bifoo[n_elements(bifoo)-1]],1), $
;            bottom=128
;    imdisp, rotate(bigmapim[*,bifoo[0]:bifoo[n_elements(bifoo)-1]],1), $
;            axis=1, xr=[tkm(birmin),tkm(birmax)], xtit='Radius'+tkmtit(), $
;            ytickn=replicate(' ',20), ytickle=1e-10, bottom=128
  endif
  ;if i eq 56 then stop
endfor

redfac = 4.  ; Make sure this is a float!
rem = ( nradi2/redfac mod 1 )*redfac
nradi3 = ( nradi2 - rem )/redfac
radidps3 = rebin( radidps2[0:nradi2-1-rem], nradi3 )
bigmap3 = rebin( bigmap[*,0:nradi2-1-rem], rx, nradi3 )
bigmap3_bksub = rebin( bigmap_bksub[*,0:nradi2-1-rem], rx, nradi3 )
bigmapim3 = rebin( bigmapim[*,0:nradi2-1-rem], rx, nradi3 )
bigmapim3_bksub = rebin( bigmapim_bksub[*,0:nradi2-1-rem], $
                         rx, nradi3 )
bestimage3 = rebin( long(strmid(bestimage2[0:nradi2-1-rem],1,10)), $
                    nradi3, /sample )
textures3 = rebin( fix(textures2[0:nradi2-1-rem]), nradi3, /sample )

justplots:
rad1 = 74490.0d0
rad2 = 76775.0d0
;rad1 = 84552.0d0
;rad2 = 85000.0d0
gap = 20
foo = where( radidps3 gt rad1 and radidps3 le rad2, count )
plotim = bytarr( count, rx*2 + gap )
plotim[*,0:rx-1] = rotate( bigmapim3[*,foo], 3 )
plotim[*,rx+gap:rx*2+gap-1] = rotate( bigmapim3_bksub[*,foo], 3 )
ints = [0l,count-1]
intval = textures3[foo[0]]
z = 0
for n=1l,count-1 do begin
  if textures3[foo[n]] ne textures3[foo[n-1]] then begin
    ints = [ [ints], [0,count-1] ]
    z = z + 1
    ints[1,z-1] = n-1
    ints[0,z] = n
    intval = [ intval, textures3[foo[n]] ]
  endif 
endfor
if keyword_set(dolzr) then begin
  lzr, 'bigmap_dps19_'+string(rad1,fo='(I06)')+'_'+string(rad2,fo='(I06)')
  @plot_prepare
  plot_color
endif
loadct, 0
imdisp, plotim, axis=1, xr=[tkm(rad1),tkm(rad2)], $;xtit='Radius'+tkmtit(), $
        ytickn=replicate(' ',20), ytickle=1e-10, bottom=128, chars=0.5, $
        xtickle=0.1
clrnames = [ '0', 'orange()', 'green()', 'red()', 'purple()', 'blue()', $
             'ltpurple()', 'ltgray()', 'yellow()', 'cyan()' ]
for n=0,z do begin
  bar = execute( 'clr = '+clrnames[intval[n]] )
  polyfill, tkm(radidps3[foo[ints[[0,1,1,0,0],n]]]), $
                                  [0,0,1,1,0]*gap+rx, color=clr, /data  
endfor
if keyword_set(dolzr) then clzr

end
