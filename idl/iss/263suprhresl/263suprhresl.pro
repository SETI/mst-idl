if keyword_set(dolzr) then begin
  lzr, '263suprhresl170404'
  @plot_prepare
  plot_color
  ;device, decomposed=0
  device, language_level=2
  device, /cmyk
endif

restore, 'forMatt.sav'    ;UVIS Beta Cen Rev 271
uvis = smooth(dat,100,/edge_truncate)
plot, tkm(rad), uvis, xr=[123.0,124.8], /nodata, $
      xs=9, ys=9, xma=[10,10], ytit='UVIS Raw Counts (Rev271 BetCen)', $
      yr=[max(uvis),min(uvis)]
iss_soft = [ [123.13,123.34], [123.88,124.21], [124.31,124.35], [124.47,124.8] ]
iss_clump = [ [123.81,123.88], [124.21,124.31], [124.35,124.47] ]
iss_striated = [ [123.64,123.81] ]
iss_strclump = [ [123.0,123.13], [123.34,123.64] ]
rss_vo = [ [123.05,123.4], [123.6,124.6] ]
radii = [ reform(iss_soft[0,*]), reform(iss_clump[0,*]), $
          reform(iss_striated[0,*]), reform(iss_strclump[0,*]) ]
for j=0,n_elements(rss_vo[0,*])-1 do radii = [ radii, rss_vo[*,j] ]
color = [ replicate(1,n_elements(iss_soft[0,*])), $
          replicate(2,n_elements(iss_clump[0,*])), $
          replicate(3,n_elements(iss_striated[0,*])), $
          replicate(4,n_elements(iss_strclump[0,*])), $
          replicate(-1,n_elements(rss_vo)) ]
dots = [  replicate(-1,n_elements(iss_soft[0,*])), $
          replicate(-1,n_elements(iss_clump[0,*])), $
          replicate(-1,n_elements(iss_striated[0,*])), $
          replicate(-1,n_elements(iss_strclump[0,*])) ]
for j=0,n_elements(rss_vo[0,*])-1 do dots = [ dots, [1,0] ]
order = sort(radii)
radii = [ radii[order], iss_soft[1,n_elements(iss_soft[0,*])-1] ]
color = color[order]
foo = where( color eq -1, count )
for j=0,count-1 do color[foo[j]] = color[foo[j]-1]
dots = dots[order]
if dots[0] eq -1 then dots[0] = 0 else stop
foo = where( dots eq -1, count )
for j=0,count-1 do dots[foo[j]] = dots[foo[j]-1]
colorpanel = [ '', 'ltcyan', 'cyan', 'ltgreen', 'green' ]
for j=0,n_elements(radii)-2 do begin
  ;clr = green()
  foo = execute('clr = '+colorpanel[color[j]]+'()')
  case dots[j] of
    0: polyfill, radii[[j,j+1,j+1,j,j]], !y.crange[[1,1,0,0,1]], /data, co=clr
    1: begin
      pat = bytarr(200,200) + clr
      pat[90:110,90:110] = 0
      polyfill, radii[[j,j+1,j+1,j,j]], !y.crange[[1,1,0,0,1]], $
                /data, co=clr;pat=pat
    end
  endcase
endfor
for k=0,n_elements(rss_vo[0,*])-1 do begin
  polyfill, rss_vo[[0,1,1,0,0],k], !y.crange[[1,1,0,0,1]], $
            /data, /line_fill, noclip=0, orient=45, spacing=.25
endfor
;oplot, [123.81,123.81], !y.crange, l=2, thick=4
;xyouts, align=1, 123.79, 280, 'Strongly Alternating!CRadial Structure'
;xyouts, align=0, 123.83, 280, 'Softer!CRadial Structure'
oplot, [124.61,124.76,124.76,124.61,124.61], 50+10*[0,0,1,1,0]
xyouts, align=.5, mean([124.61,124.76]), 57, 'Pan 10:9 DW', chars=0.75
oplot, [124.35,124.49,124.49,124.35,124.35], 50+10*[0,0,1,1,0]
xyouts, align=.5, mean([124.35,124.49]), 57, 'Atlas 7:6 DW', chars=0.75
oplot, [124.11,124.19,124.19,124.11,124.11], 50+10*[0,0,1,1,0]
xyouts, align=.5, mean([124.11,124.19]), 57, 'Pandora 11:9 DW', chars=0.75
oplot, [123.58,123.72,123.72,123.58,123.58], 310+10*[0,0,1,1,0]
xyouts, align=.5, mean([123.58,123.72]), 317, 'Prom 6:5 DW', chars=0.75
axis, yaxis=0, yr=!y.crange, /ys, ytit='UVIS Raw Counts (Rev271 BetCen)'
axis, xaxis=0, xr=!x.crange, /xs, xtit='Radius'+tkmtit()
axis, xaxis=1, xr=!x.crange, /xs, xtickn=replicate(' ',20)
oplot, tkm(rad), uvis
axis, yaxis=1, /ys, yr=[.12,.3], ytit='ISS I/F (Rev263 SUPRHRESL)', $
      /save, co=ctred()
dir = '~/Data/images/263/SUPRHRESL/'
restore, dir+'stretch.sav'
for j=9,13 do begin
  restore, dir+strmid(filenames[j],0,17)+'.scan1'
  oplot, tkm(radi), val, co=ctred()
endfor

if keyword_set(dolzr) then clzr

end
