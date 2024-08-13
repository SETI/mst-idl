!p.multi=[0,3,1]
!x.margin=[0,0]
!x.omargin=[3,3]

if keyword_set(dolzr) then begin
  lzr, 'jemodelpredict', aspect=.45
  @plot_prepare
endif

savefile1=[ '028_rdhresscn001', '028_rdhresscn002', '031_rdhrcomp', $
            '032_rdhrcomp', '046_rdhresscn', '056_rdhresscn' ] + '.sav'
savefile1 = clip(savefile1)
savefile1 = [ 'soi_soispturn', '008_rdhrcomp', savefile1 ]

nr = 2000
nk = n_elements(savefile1)
rl1 = [ 7, 9, 11 ]
rl2 = [ 5, 7, 9 ]
sigma = [ 11.5, 43, 43 ]
xi_d = [ 8, 8.5, 6 ]
xr = [ 121.345, 128.425, 132.73 ]
for j=0,2 do begin
  savefile2 = 'jemodelpredict'+strtrim(rl1[j],2)+strtrim(rl2[j],2)+'.sav'
  if keyword_set(findfile(savefile2)) then begin
    restore, savefile2
    year = strmid(image_mid_time,0,4)
  endif else begin
    _rr = dblarr(nr,nk)
    _dw = dblarr(nr,nk)
    _dw2 = dblarr(nr,2,3,nk)
    image_mid_time = strarr(nk)
    imlon = dblarr(nk)
    for k=0,nk-1 do begin
      if k eq 0 then begin
        image_mid_time[k] = ([ '2004-183T03:32:10.283', $
                               '2004-183T03:32:10.283', $
                               '2004-183T03:32:10.283' ])[j]
        imlon[k] = ([ 320.07718, 320.07718, 320.07718 ])[j]
        dradi = 0.25
      endif else if k eq 1 then begin
        image_mid_time[k] = ([ '2005-141T00:18:04.762', $
                               '2005-141T00:05:56.766', $
                               '2005-140T23:56:54.770' ])[j]
        imlon[k] = ([ 280.89061, 278.94047, 277.70155 ])[j]
        dradi = 1.4
      endif else begin
        xi_d = [ 6, 8.5, 6 ]
        restore, savefile1[k]
        year = strmid(date,0,4)
        foo = doy( strmid(date,5,3), month=month, day=day )
        hour = strmid(date,9,2)
        minute = strmid(date,12,2)
        second = strmid(date,15,6)
        jd = julday( month, day, year, hour, minute, second )
        dt = ([ t75, t97, t119 ])[j]
        image_mid_time[k] = caldate( jd + dt/60/24, /doy )
        imlon[k] = ([ l75, l97, l119 ])[j]
        if k eq 5 then dradi = 0.7 else dradi = 1.4
      endelse
      if keyword_set(dradi) then resample = 1 else resample = 0
      jemodel, rl1[j], rl2[j], rr, dw, sigma=sigma[j], xi_d=xi_d[j], nr=nr, $ 
           image_mid_time=image_mid_time[k], imlon=imlon[k], decay=1, $
           dw2=dw2, tit='J/E '+strtrim(rl1[j],2)+':'+strtrim(rl2[j],2)+$
               ', '+savefile1[k], dradi=dradi, resample=resample
      _rr[*,k] = rr 
      _dw[*,k] = dw 
      _dw2[*,*,*,k] = dw2 
    endfor
    save, _rr, _dw, _dw2, image_mid_time, imlon, filename=savefile2
  endelse
  yy = 12
  !p.charsize = 1.5
  plot, [ tkm(min(_rr)), xr[j] ], [-8-(nk-1)*yy,15], /nodata, /xs, /ys, $
        xtickle=1e-10, ytickle=1e-10, ytickn=replicate(' ',20), $
        xtit='Radius'+tkmtit()
  axis, xaxis=0, xtickn=replicate(' ',20), /data, /xs
  for k=0,nk-1 do oplot, tkm(_rr[*,k]), _dw[*,k]-k*yy
  if j eq 0 then for k=0,nk-1 do begin
    xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])/20, -k*yy-4, chars=1, $
            strmid(image_mid_time[k],0,4)+' '+$
            doy(strmid(image_mid_time[k],5,3),$
            leap=1-sign( strmid(image_mid_time[k],0,4) mod 4 ),$
                /short)
  endfor
  xyouts, mean(!x.crange), !y.crange[1] - 6, align=.5, $
          'J/E '+strtrim(rl1[j],2)+':'+strtrim(rl2[j],2)
endfor

if keyword_set(dolzr) then clzr

end
