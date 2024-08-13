basedir = '$DATA/images/'
; All of these images have been carefully hand-pointed with stars. 
; Errors are ~1 pixels for lophase and ~5 pixels for hiphase. 
dirs = [ '072/RHEARPXLP/', '087/RHEARPXLP/', '100/RHEARPXHP/', $
         '104/RHEARPXLP/', '109/RHEARPXLP/' ]
ndirs = n_elements(dirs)

for k=0,ndirs-1 do begin
  restore, basedir + dirs[k] + 'stretch.sav'
  restore, basedir + dirs[k] + 'spreadsheet.sav'
  nfiles = n_elements(data[0,*])
  data = [ data, replicate(dirs[k],1,nfiles) ]
  if k eq 0 then begin
    ssheet = data
    phase = _keywords.ringplane_aimpoint_phase_angle
    emission = _keywords.ringplane_aimpoint_emission_angle
    incidence = _keywords.ringplane_aimpoint_incidence_angle
  endif else begin
    ssheet = [ [ssheet], [data] ]
    phase = [ phase, _keywords.ringplane_aimpoint_phase_angle ]
    emission = [ emission, _keywords.ringplane_aimpoint_emission_angle ]
    incidence = [ incidence, _keywords.ringplane_aimpoint_incidence_angle ]
  endelse  
endfor
use = where( ( ssheet[3,*] eq '18000' or phase gt 90 ) and $
             ssheet[7,*] eq 'FULL' and ssheet[0,*] ne 'N1619427494_1', nuse )

if keyword_set(movie) then for jjj=0,nuse-1 do begin
  image_name = basedir + ssheet[14,use[jjj]] + ssheet[0,use[jjj]] + '_cal.IMG'
  print, '------'
  print, strtrim(jjj,2) + ' / ' + strtrim(nuse,2) + '  ' + $
         ssheet[14,use[jjj]] + ssheet[0,use[jjj]]
  print, '------'
  @caviar
  @rhea_rings
endfor

lims = fltarr(2,ndirs)
for k=0,ndirs-1 do begin
  lims[0,k] = min(where( ssheet[14,use] eq dirs[k] ))
  lims[1,k] = max(where( ssheet[14,use] eq dirs[k] ))
endfor

if keyword_set(plotphotangles) then begin
  if keyword_set(dolzr) then begin
    lzr, 'rhea_images_photangles', /half
    @plot_prepare
  endif
  !p.multi = [0,2,3]
  !p.charsize = 2
  !y.margin = 0
  !y.omargin = [4,2]
  notn = replicate(' ',20)
  solid_small_circles
  yr1a = [150,155]
  yr1 = [3,18]
  yr2 = [-2.8,3.8]
  yr3 = [91,97]
  xr = [0,nuse]-0.5
  yht = ( float(!d.y_size)/!d.y_ch_size - 6 )/3
  plot, phase[use], ps=8, xtickle=1e-10, xtickn=notn, $ ;ytit='Phase', $
        xr=xr, yr=yr1, xs=5, /ys, yma=[0,0.3]*yht, yticki=3
  oplot, !x.crange, yr1[[1,1]], l=1, /noclip
  xyouts, !x.crange[0] - (!x.crange[1]-!x.crange[0])*.15, $
          !y.crange[0] + (!y.crange[1]-!y.crange[0])/1.4, $
          'Phase Angle (!Uo!N)', orient=90, align=.5, /noclip, chars=1
  !p.multi[0] = !p.multi[0] + 1
  plot, phase[use], ps=8, xtickle=1e-10, xtickn=notn, $ ;ytit='Phase', $
        xr=xr, yr=yr1a, xs=5, /ys, yma=[0.75,0]*yht, yticki=3, tit='RHEARPX'
  oplot, !x.crange, yr1a[[0,0]], l=1
  axis, xaxis=1, xtickn=notn, xtickle=1e-10
  !p.multi[0] = !p.multi[0] - 1
  plot, emission[use]-90, ps=8, xtickle=1e-10, xtickn=notn, $
        ytit='Opening Angle (!Uo!N)', xr=xr, yr=yr2, /xs, /ys
  !p.multi[0] = !p.multi[0] - 1
  plot, incidence[use], ps=8, xtickle=1e-10, $
        xtickv=rebin(lims,1,ndirs), xtickn=strmid(dirs,0,3), xticks=ndirs-1, $
        xr=xr, yr=yr3, /xs, /ys, ytit='Incidence Angle (!Uo!N)'
  for k=0,ndirs-2 do oplot, mean([lims[1,k],lims[0,k+1]])*[1,1], $
                            !y.crange[0] + (!y.crange[1]-!y.crange[0])*[0,3], $
                            /noclip, l=1
  if keyword_set(dolzr) then clzr
endif

if keyword_set(movehere) then begin
  for j=0,nuse-1 do begin
    spawn, 'mv '+basedir+ssheet[14,use[j]]+ssheet[0,use[j]]+'* .'
  endfor
  spreadsheet = ssheet[*,use]
  save, spreadsheet, legend, filename='spreadsheet.sav'
endif

end
