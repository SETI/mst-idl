!y.omargin = [4,2]
!y.margin = 0
if keyword_set(rss) then instr = 'RSS' else instr = 'PPS'
if keyword_set(dolzr) then begin
  lzr, '/home/borogove/iss/images/Voyager/VGR_'+instr+$
       '/KM001/vgr_'+strlowcase(instr)
  @plot_prepare
endif
for base=70,140,10 do begin
  if base lt 100 then sbase='0'+strtrim(base,2) else sbase=strtrim(base,2)
  restore, '/home/borogove/iss/images/Voyager/VGR_'+instr+$
           '/KM001/vgr_'+strlowcase(instr)+'_'+sbase+'.sav'
  r = *data.radius
  tau = *data.tau
  r = r[where( tau lt 5 )]
  tau = tau[where( tau lt 5 )]
  !p.multi = [0,1,10]
  for j=0,9 do begin
    if j eq 0 then tit='Voyager '+instr+': '+strtrim(base,2)+',000 to '+$
                   strtrim(base+10,2)+',000 km' else tit=''
    if j eq 9 then xtn='' else xtn=replicate(' ',20)
    if j eq 9 then xtit='Additional Radius (km)' else xtit=''
    plot, r-(float(base)+j)*1000, tau, /xs, /ys, $
          yminor=1, ytickle=!p.ticklen/2, $
          xr=[0,1000], tit=tit, xtit=xtit, xtickn=xtn, ytit='!Mt'
    xyouts, !x.crange[0] + (!x.crange[1]-!x.crange[0])*.025, $
            !y.crange[1] - (!y.crange[1]-!y.crange[0])*.2, $
            strtrim(base+j,2)+',000 km'
  endfor
  if !d.name eq 'X' then stop
endfor
if keyword_set(dolzr) then clzr

end
