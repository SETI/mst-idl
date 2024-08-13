if keyword_set(zscale) then begin
  savefile = 'rhea_reproject_zscale.sav'
endif else if keyword_set(sm) then begin
  savefile = 'rhea_reproject_'+strtrim(sm,2)+'.sav'
endif else begin
  savefile = 'rhea_reproject.sav'
endelse
restore, savefile
print, 'Restoring '+savefile
restore, 'spreadsheet.sav'
sz = size(__imrz)
xs = sz[1]
ys = sz[2]
if keyword_set(paper_plot) then begin
  nx = 2
  ny = 4
endif else begin
  nx = 1;2
  ny = 1;5
endelse
marg = 30
;window, xs=(xs+marg)*nx, ys=(ys+marg)*ny+marg*0.5
thresh = replicate(3e-7,nf)
rms = fltarr(nf)
for j=0l,nf do begin
  if ( j mod (nx*ny) eq 0 and j ne 0 ) or j eq nf then begin
    if j eq nf then begin
      ; Sample ring width
      rrhea = xs/(6-1.5)
      ycen = marg*0.5 + 0.5*ys
      theta = findgen(361)*!pi/180
      restore, 'rhea_reproject_ringwidth.sav'
      foo = where( strmid(spreadsheet[14,*],11,1) eq 'L' )
      ww = [mean(width[foo]),max(width[foo])]
      tt = 'Rhea Ring Contours for '+[ 'an average low-phase image', $
               'highest low-phase opening angle used ('+$
               strmid(spreadsheet[0,where(width eq max(width[foo]))],0,11)+')' ]
      for k=0,1 do begin
        xcen = marg*0.5+(xs+marg)*k - 1.5*rrhea
;        tv, fltarr(xs,ys)+100, marg*0.5+(xs+marg)*k, marg*0.5
        for r=2,4 do begin
          xx = xcen + r*rrhea*sin(theta)
          yy = ycen + ww[k]*cos(theta)
          foo = where( xx gt marg*0.5+(xs+marg)*k )
;          plots, /device, xx[foo], yy[foo]
;          xyouts, /device, align=0.5, chars=1.5, $
;                  marg*0.5+(xs+marg)*k + xs*0.5, marg*0.5 + ys + marg/4, tt[k]
        endfor
      endfor 
    endif 
;    if keyword_set(dolzr) then begin
;      if j mod (nx*ny) eq 0 then num = j/nx/ny else num = j/nx/ny + 1
;      tifffile = 'rhea_reproject_display'+strtrim(num,2)
;      if keyword_set(zscale) then tifffile = tifffile + '_zscale'
;      tifffile = tifffile +'.tiff'
;      outim = tvrd()
;      write_tiff, tifffile, reverse( outim, 2 )
;    endif else stop
    if j eq nf then goto, next
;    tv, replicate( 0, (xs+marg)*nx, (ys+marg)*ny+marg*0.5 )
  endif 
  foo = where( __rr[*,j] ge 2 and __rr[*,j] le 3 )
  rms[j] = stddev((__imrz[foo,*,j])[where(__imrz[foo,*,j] ne 0)])
  thresh[j] = rms[j]/2
;  tvscl, __imrz[*,*,j]>(-thresh[j])<thresh[j], $
;         marg*0.5+(xs+marg)*(j mod nx), $
;         marg*0.5+(ys+marg)*((nx*ny-1-(j mod (nx*ny)))/nx)
;  xyouts, /device, align=0.5, chars=1.5, $
;          marg*0.5+(xs+marg)*(j mod nx) + xs*0.5, $
;          marg*0.5+(ys+marg)*((nx*ny-1-(j mod (nx*ny)))/nx) + ys + marg/4, $
;          spreadsheet[14,j]+strmid(spreadsheet[0,j],0,11)+' ('+strtrim(j,2)+')'
endfor
next:

;window
lims = [ [0,5], [6,11], [12,25], [26,31], [32,39], [40,47], [48,64] ]
group = [ '072/RHEARPXLP 1', '072/RHEARPXLP 2', '087/RHEARPXLP', $
          '100/RHEARPXHP', '104/RHEARPXLP 1', '104/RHEARPXLP 2', $
          '109/RHEARPXLP' ]
;groupshort = [ '072-1', '!C072-2', '087', '100', '!C104-1', '104-2', '109' ]
lims1 = [ [0,11], [12,25], [26,31], [32,47], [48,64] ]
groupshort = [ '072', '087', '100', '104', '109' ]
ngrp = n_elements(group)
ngrp1 = n_elements(groupshort)

ntot = 4
rms_grp = fltarr(ngrp)
rms_tot = fltarr(ntot)
savefile = 'rhea_reproject_grp'
if keyword_set(zscale) then savefile = savefile + '_zscale'
savefile = savefile + '.sav'
if keyword_set(findfile(savefile)) then restore, savefile else begin
  imrz_grp = fltarr(xs,ys,ngrp)
  imrz_tot = fltarr(xs,ys,ntot)
  for k=0,ngrp-1 do begin
    print, strtrim(k,2)+' / '+strtrim(ngrp+ntot,2)
    use = lindgen(lims[1,k]-lims[0,k]+1) + lims[0,k]
    for x=0l,xs-1 do for y=0l,ys-1 do begin
      foo = where( __imrz[x,y,use] ne 0, count )
      if count gt 0 then begin
        imrz_grp[x,y,k] = mean(__imrz[x,y,use[foo]])
      endif 
    endfor
    foo = where( __rr[*,use[0]] ge 2 and __rr[*,use[0]] le 3 )
    rms_grp[k] = stddev((imrz_grp[*,*,k])[where(imrz_grp[*,*,k] ne 0)])
  endfor
  for k=0,2,2 do begin
    print, strtrim(ngrp+k,2)+' / '+strtrim(ngrp+ntot,2)
    use = lindgen(nf)
    if k eq 2 then begin
      use = vec_remove( use, where( spreadsheet[14,*] eq '100/RHEARPXHP/' ) )
      if n_elements(use) ne nf-6 then stop
    endif 
    for x=0l,xs-1 do for y=0l,ys-1 do begin
      foo = where( __imrz[x,y,use] ne 0, count )
      if count gt 0 then begin
        imrz_tot[x,y,k] = mean(__imrz[x,y,use[foo]] )
        ;imrz_tot[x,y,k] = total(__imrz[x,y,use[foo]] / rms[use[foo]] ) / $
        ;                  total( 1 / rms[use[foo]] )
      endif 
    endfor
    foo = where( __rr[*,0] ge 2 and __rr[*,0] le 3 )
    rms_tot[k] = stddev((imrz_tot[*,*,k])[where(imrz_tot[*,*,k] ne 0)])
  endfor
  for k=1,3,2 do begin
    print, strtrim(ngrp+k,2)+' / '+strtrim(ngrp+ntot,2)
    use = lindgen(ngrp)
    if k eq 3 then begin
      use = vec_remove( use, where( group eq '100/RHEARPXHP' ) )
      if n_elements(use) ne ngrp-1 then stop
    endif 
    for x=0l,xs-1 do for y=0l,ys-1 do begin
      foo = where( imrz_grp[x,y,use] ne 0, count )
      if count gt 0 then begin
        imrz_tot[x,y,k] = mean( imrz_grp[x,y,use[foo]] )
        ;imrz_tot[x,y,k] = total( imrz_grp[x,y,use[foo]] / rms_grp[use[foo]] )/$
        ;                  total( 1 / rms_grp[use[foo]] )
      endif 
    endfor
    foo = where( __rr[*,0] ge 2 and __rr[*,0] le 3 )
    rms_tot[k] = stddev((imrz_tot[*,*,k])[where(imrz_tot[*,*,k] ne 0)])
  endfor 
  save, imrz_grp, imrz_tot, group, lims, ngrp, ntot, rms, rms_grp, rms_tot, $
        filename=savefile
endelse

if keyword_set(dolzr) then begin
  psname = 'rhea_reproject_rms'
  if keyword_set(zscale) then psname = psname + '_zscale'
  lzr, psname, /half
  @plot_prepare
  plot_color
endif
!p.multi = [0,2,3]
!p.charsize = 2
!y.margin = 0
!y.omargin = [4,2]
notn = replicate(' ',20)
solid_small_circles
yr1a = [151,155]
yr1 = [3,15]
yr2 = [-2.8,3.8]
yr3 = [91,97]
xr = [0,nf]-0.5
yht = ( float(!d.y_size)/!d.y_ch_size - 6 )/3
if not keyword_set(_keywords) then restore, 'stretch.sav'
phase = _keywords.ringplane_aimpoint_phase_angle
emission = _keywords.ringplane_aimpoint_emission_angle
incidence = _keywords.ringplane_aimpoint_incidence_angle
range = _keywords.primary_distance / 60330
plot, phase, ps=8, xtickle=1e-10, xtickn=notn, $ ;ytit='Phase', $
      xr=xr, yr=yr1, xs=5, /ys, yma=[0,0.3]*yht, yticki=2
xyouts, !x.crange[0]+(!x.crange[1]-!x.crange[0])*.05, $
        !y.crange[0]+(!y.crange[1]-!y.crange[0])*.1*4/3, '(a)', chars=1
oplot, !x.crange, yr1[[1,1]], l=1, /noclip
xyouts, !x.crange[0] - (!x.crange[1]-!x.crange[0])*.15, $
        !y.crange[0] + (!y.crange[1]-!y.crange[0])/1.4, $
        'Phase Angle (!Uo!N)', orient=90, align=.5, /noclip, chars=1
axis, xaxis=0, xtickn=notn, xtickle=1e-10
for k=0,ngrp1-2 do oplot, mean([lims1[1,k],lims1[0,k+1]])*[1,1], !y.crange, l=1
!p.multi[0] = !p.multi[0] + 1
plot, phase, ps=8, xtickle=1e-10, xtickn=notn, $ ;ytit='Phase', $
      xr=xr, yr=yr1a, xs=5, /ys, yma=[0.75,0]*yht, yticki=2;, tit='RHEARPX'
oplot, !x.crange, yr1a[[0,0]], l=1
axis, xaxis=1, xtickn=notn, xtickle=1e-10
for k=0,ngrp1-2 do oplot, mean([lims1[1,k],lims1[0,k+1]])*[1,1], !y.crange, l=1
plot, range, xr=xr, /xs, yr=[7,23], /ys, ps=8, xtickle=1e-10, $
      ytit='Range (R!DS!N)', xtickn=notn
for k=0,ngrp1-2 do oplot, mean([lims1[1,k],lims1[0,k+1]])*[1,1], !y.crange, l=1
xyouts, !x.crange[0]+(!x.crange[1]-!x.crange[0])*.05, $
        !y.crange[0]+(!y.crange[1]-!y.crange[0])*.1, '(b)', chars=1
plot, emission-90, ps=8, xtickle=1e-10, xtickn=notn, $
      ytit='Opening Angle (!Uo!N)', xr=xr, yr=yr2, /xs, /ys
for k=0,ngrp1-2 do oplot, mean([lims1[1,k],lims1[0,k+1]])*[1,1], !y.crange, l=1
xyouts, !x.crange[0]+(!x.crange[1]-!x.crange[0])*.05, $
        !y.crange[0]+(!y.crange[1]-!y.crange[0])*.1, '(c)', chars=1
plot, width, xr=xr, /xs, yr=[1,19], /ys, ps=8, xtickle=1e-10, $
      ytit='Ring Width (pixels)', xtickn=notn
for k=0,ngrp1-2 do oplot, mean([lims1[1,k],lims1[0,k+1]])*[1,1], !y.crange, l=1
xyouts, !x.crange[0]+(!x.crange[1]-!x.crange[0])*.05, $
        !y.crange[0]+(!y.crange[1]-!y.crange[0])*.1, '(d)', chars=1
plot, incidence, ps=8, xtickle=1e-10, $
      xtickv=rebin(float(lims1),1,ngrp1), xtickn=groupshort, xticks=ngrp1-1, $
      xr=xr, yr=yr3, /xs, /ys, ytit='Incidence Angle (!Uo!N)', $
      xtit='Orbit Number'
for k=0,ngrp1-2 do oplot, mean([lims1[1,k],lims1[0,k+1]])*[1,1], !y.crange, l=1
xyouts, !x.crange[0]+(!x.crange[1]-!x.crange[0])*.05, $
        !y.crange[0]+(!y.crange[1]-!y.crange[0])*.1, '(e)', chars=1
plot, rms, xr=xr, /xs, yr=[1e-7,2e-5], /ys, ps=8, xtickle=1e-10, $
      xtickv=rebin(float(lims1),1,ngrp1), xtickn=groupshort, xticks=ngrp1-1, $
      ytit='RMS I/F', /ylog, xtit='Orbit Number'
for k=0,ngrp1-2 do oplot, mean([lims1[1,k],lims1[0,k+1]])*[1,1], 10^!y.crange, $
                          l=1
xyouts, !x.crange[0]+(!x.crange[1]-!x.crange[0])*.05, $
        10^(!y.crange[0]+(!y.crange[1]-!y.crange[0])*.1), '(f)', chars=1

solid_triangles
oplot, rebin(float(lims),1,ngrp), rms_grp, ps=8
solid_small_circles
if keyword_set(dolzr) then clzr else stop
stop
!p.font = -1
if keyword_set(paper_plot) then begin
  if keyword_set(dolzr) then begin
    lzr, 'rhea_reproject_grp'
    @plot_prepare
    plot_color
  endif else begin
    window, xs=(xs+marg)*nx, ys=(ys+marg)*ny
    ;tv, replicate( 255, (xs+marg)*nx, (ys+marg)*ny+marg*0.5 )
    !p.multi = [0,2,4]
  endelse
endif else begin
  window, xs=(xs+marg)*nx, ys=(ys+marg)*ny+marg*0.5
endelse
for j=0l,ngrp+ntot do begin
  if ( j mod (nx*ny) eq 0 and j ne 0 ) or j eq ngrp+ntot then begin
    if keyword_set(dolzr) then begin
      if j mod (nx*ny) eq 0 then num = j/nx/ny else num = j/nx/ny + 1
      tifffile = 'rhea_reproject_grp_display'+strtrim(num,2)
      if keyword_set(zscale) then tifffile = tifffile + '_zscale'
      if keyword_set(paper_plot) then begin
      endif else begin
        tifffile = tifffile +'.tiff'
        outim = tvrd()
        write_tiff, tifffile, reverse( outim, 2 )
      endelse
    endif else stop
    if j eq ngrp+ntot then goto, next2
    ;if j eq nx*ny then goto, next2
    if not keyword_set(paper_plot) then begin
      ;tv, replicate( 0, (xs+marg)*nx, (ys+marg)*ny+marg*0.5 )
    endif
  endif 
  if j lt ngrp then begin
    if keyword_set(paper_plot) then begin
      ; Load color table 15 and fix it
      if !d.name eq 'X' then device, decomposed=0
      loadct, 15
      tvlct, r, g, b, /get
      r[where(r lt g)] = g[where(r lt g)]
      tvlct, r, g, b
      thresh = 5e-6
      if group[j] eq '100/RHEARPXHP' then thresh = thresh / 10
    endif else thresh = rms_grp[j]/2
    case j of
      0: jj=1
      1: jj=2
      2: jj=3
      3: jj=7
      4: jj=4
      5: jj=5
      6: jj=6
    endcase 
    if jj ge 5 then begin
      xtit = 'Distance (R!DR!N)'
      xtn = ''
    endif else begin
      xtit = ''
      xtn = notn
    endelse 
    if jj eq 2 or jj eq 4 or jj eq 6 then begin
      ytit = ''
      ytn = notn
    endif else begin
      ytit = 'Distance (R!DR!N)'
      ytn = ''
    endelse 
    imdisp, imrz_grp[*,*,j]>(-thresh)<thresh, /axis, xtit=xtit, ytit=ytit, $
            xtickn=xtn, ytickn=ytn, xtickle=1e-10, ytickle=1e-10, $
            xr=[ min(__rr[*,lims[0,j]]), max(__rr[*,lims[0,j]]) ], $
            yr=[-100,100]*range[j]*60330*6e-6/764, /xs, /ys, $
            pos=[ 0.47*((jj+1) mod 2), (8-jj)/2*0.16+(jj ne 7)*0.05, $
                  0.5+0.47*((jj+1) mod 2), 0.2+(8-jj)/2*0.16+(jj ne 7)*0.05 ]
    if keyword_set(paper_plot) then begin
      if j eq 3 then hilo = 'High' else hilo = 'Low'
      xyouts, !x.crange[1] - (!x.crange[1]-!x.crange[0])*.02, $
              !y.crange[1] - (!y.crange[1]-!y.crange[0])*.15, $
              'Orbit '+strmid(group[j],0,3)+' ('+hilo+' Phase)', /align, /chars
      if j eq 6 then goto, next2
    endif else begin
      xyouts, /device, align=0.5, chars=1.5, $
              marg*0.5+(xs+marg)*(j mod nx) + xs*0.5, $
              marg*0.5+(ys+marg)*((nx*ny-1-(j mod (nx*ny)))/nx) + ys + marg/4, $
              group[j]
    endelse 
  endif else if j mod (nx*ny) ge nx*ny-2 then begin
    jj = j-ngrp;+1
    thresh = stddev((imrz_tot[*,*,jj])[where(imrz_tot[*,*,jj] ne 0)])
    tvscl, imrz_tot[*,*,jj]>(-thresh)<thresh, $
           marg*0.5+(xs+marg)*(j mod nx), $
           marg*0.5+(ys+marg)*((nx*ny-1-(j mod (nx*ny)))/nx)
    if not keyword_set(paper_plot) then begin
      xyouts, /device, align=0.5, chars=1.5, $
              marg*0.5+(xs+marg)*(j mod nx) + xs*0.5, $
              marg*0.5+(ys+marg)*((nx*ny-1-(j mod (nx*ny)))/nx) + ys + marg/4, $
              'Low-Phase Total '+strtrim(j-ngrp,2)
    endif 
    next3:
  endif 
endfor
next2:

if keyword_set(paper_plot) then begin
  !p.multi[0] = 1 + !p.multi[1]*!p.multi[2] - 8
  scalebar = rebin(rebin( indgen(256), 256, 1 ),256,10)
  imdisp, scalebar, xs=9, /axis, xr=[-thresh,thresh], ytickn=notn, $
          xtickle=1e-10, ytickle=1e-10, pos=[ 0.52, 0.05, 0.92, 0.13 ], $
          xtit='Image Brightness, Low Phase (I/F)'
  axis, xaxis=1, /xs, xr=[-thresh,thresh]/10, xtickle=1e-10
  xyouts, 0, 30, align=0.5, /noclip, 'Image Brightness, High Phase (I/F)', $
          chars=1
  if keyword_set(dolzr) then clzr
endif

end
