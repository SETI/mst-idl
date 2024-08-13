if keyword_set(paperplot) then begin
  rimp = 1
  flux = 1
endif
dirs = [ '007/HIPHASE', '116/EQXSHADOW001', '116/EQXSHADOW005', $
         '116/SATELLORB', '116/EQXSHADOW013', '169/HIPHASE' ]
ndirs = n_elements(dirs)
basedir = '$DATA/images/'
image_range = lonarr(2,ndirs)
for k=0,ndirs-1 do begin
  restore, basedir + dirs[k] + '/image_outlines.sav'
  restore, basedir + dirs[k] + '/image_coverage_area.sav'
  image_range[0,k] = min(image_num)
  image_range[1,k] = max(image_num)
  if k eq 0 then _area = area else _area = [ [_area], [area] ]
endfor

ringedges = [ 74490.0d0, 91980, 117500, 122050, 136770 ]
totalarea = !dpi*( ringedges[1:4]^2 - ringedges[0:3]^2 )
totalarea = [ totalarea, !dpi*( ringedges[4]^2 - ringedges[0]^2 ) ]

detected = [ [ 1, 0, 0, 0, 0 ], $
             [ 0, 1, 0, 0, 0 ], $
             [ 1, 0, 0, 0, 0 ], $
             [ 0, 0, 0, 1, 0 ], $
             [ 0, 0, 0, 1, 0 ], $
             [ 1, 0, 0, 0, 0 ] ]
expected = detected/_area*rebin(totalarea,5,ndirs)
expected[where( _area eq 0 )] = 0

location = [ 'C', 'C', 'C', 'Bq', 'Cq', 'Cq', 'Aq', 'C', 'A' ]
if not keyword_set(qq) then qq=4
;case qq of
;  3:  ea = [ 0.03, 0.14, 1.2, 50, 1.2, 1e5, 1.5, 1.5 ]
;  4:  ea = [ 0.025, 0.11, 1, 40, .68, 4e3, 1.6, 1.6 ]
;endcase
; These are values of rimp from impactorsizecalcs
if keyword_set(rimp) then begin
  ;ea = [ .027, .05, .077, .27, .4, 2.25, .09, .09 ]
  ea3 = [ .11, .22, .3, .78, 2.3, .62, 13.9, .33, .33 ]  ; q=3
  ea = [ .023, .045, .06, .15, .43, .12, 2.1, .067, .067 ]  ; q=4
  ; rimp = A mcloud^(1/3) ==> sigma_rimp = (A/3) * sigma_mcloud / mcloud^(2/3)
  ; ==> sigma_rimp / rimp = (1/3) * sigma_mcloud / mcloud
  ea_sigma = [ .002/.014, .002/.015, .004/.340, .2/4.6, .01/.32, $
               .01/.32, 5./3643, .02/.35, .02/.35 ] / 3 * ea
endif
expnum = [ expected[0,0]*5, expected[0,0]*2, expected[0,0], expected[1,1], $
           expected[0,2], expected[0,2], expected[3,3], expected[0,5], $
           1/_area[3,5]*totalarea[3] ]
if keyword_set(flux) then expnum = expnum / totalarea[[0,0,0,1,0,2,2,0,2]]/1e6
age = [ 1, 1, 1, 4, 30, mean([10.9,15]), 23.5, .86, .86 ]*3600
if keyword_set(flux) then expnum = expnum / age
;limit = [ '', '', '', '', 'EA', 'EA/#', '', '#' ]
limit = [ '', '', '', '', 'EA', 'EA', '#', '', '#' ]

if keyword_set(tex) then begin
  for k=0,ndirs-1 do begin
    areapct = _area[*,k]/totalarea*100
    print, image_range[*,k], $
           fix(areapct[0]), fix(( areapct[0] - fix(areapct[0]) )*10), $
           fix(areapct[1]), fix(( areapct[1] - fix(areapct[1]) )*10), $
           fix(areapct[3]), fix(( areapct[3] - fix(areapct[3]) )*10), $
           fo='("N",I10,"--N",I10," ('+(['C1--C5','B','C','A1','A2','C6'])[k]+$
           ') & ~",I2," & ",I1,"\% & ~",I2," & ",I1,"\% & ~",I2," & ",I1,"\% \\")'
  endfor 
  stop
endif

print, ''
print, 'Coverage area in millions of km^2:'
heading = replicate(' ',20)
for q=0,n_elements(arealegend)-1 do begin
  heading = [ heading, replicate(' ',10-strlen(arealegend[q])), arealegend[q] ]
endfor
print, strjoin(heading)
for k=0,ndirs-1 do begin
  print, _area[*,k]/1e6, $
         fo='("'+strjoin([dirs[k],replicate(' ',20-strlen(dirs[k]))])+$
            '",F10.2,F10.2,F10.2,F10.2,F10.2)'
endfor
print, ''
print, 'Fractional coverage area:'
heading = replicate(' ',20)
for q=0,n_elements(arealegend)-1 do begin
  heading = [ heading, replicate(' ',10-strlen(arealegend[q])), arealegend[q] ]
endfor
print, strjoin(heading)
for k=0,ndirs-1 do begin
  print, _area[*,k]/totalarea*100, $
         fo='("'+strjoin([dirs[k],replicate(' ',20-strlen(dirs[k]))])+$
            '",F9.2,"%",F9.2,"%",F9.2,"%",F9.2,"%",F9.2,"%")'
endfor
print, ''
print, 'Expected number of ejecta clouds, given detection:'
heading = replicate(' ',20)
for q=0,n_elements(arealegend)-1 do begin
  heading = [ heading, replicate(' ',10-strlen(arealegend[q])), arealegend[q] ]
endfor
print, strjoin(heading)
for k=0,ndirs-1 do begin
  print, expected[*,k], $
         fo='("'+strjoin([dirs[k],replicate(' ',20-strlen(dirs[k]))])+$
            '",F10.2,F10.2,F10.2,F10.2,F10.2)'

endfor
print, ''

if keyword_set(dolzr) then begin
  lzr, 'impact_image_coverage_area', /half
  @plot_prepare
  plot_color
  !p.charsize = 2
endif

if qq eq 3 then xmax=1e6 else if qq eq 4 then xmax=1e5
if keyword_set(rimp) then xmax=19
if keyword_set(rimp) then xs=5 else xs=1
if keyword_set(rimp) then xarr=1.5 else xarr=2.5
if keyword_set(rimp) then xtit='Impactor Radius (m)' else begin
  xtit='Initial Equivalent Area (km!U2!N)'
endelse
if keyword_set(flux) then ytit='Estimated Influx Rate !MF (m!U-2!N s!U-1!N)'
if keyword_set(flux) then yr=[1e-20,1e-16] else yr=[6,2000]
if keyword_set(flux) then yarr=2 else yarr=1.5
; Solid line from Figure 17a of Cuzzi and Estrada (1998, Icarus)
ce98x = [ 10^.625/10, 10^.875/10, 10^.125, 10^.375 ]/100
ce98y = sqrt(10)*10.^[-14,-15,-16,-17]
plot, [3e-3,xmax], yr, /nodata, /xlog, /ylog, xs=xs, /ys, xtit=xtit, ytit=ytit
polyfill, [ce98x,reverse(ce98x)/mean(ea3/ea)], [ce98y,reverse(ce98y)], $
          co=ltgray(), noclip=0
if keyword_set(rimp) then begin
  axis, xaxis=1, /xs, /xlog, xr=10^!x.crange, xtit=xtit+' if q=4'
  axis, xaxis=0, /xs, /xlog, xr=10^!x.crange*mean(ea3/ea), xtit=xtit+' if q=3'
endif
dd0 = .9
dd1 = .07
dd2 = .015
dd3 = .55
dd4 = .03
oplot, 10^( !x.crange[0] + $
            (!x.crange[1]-!x.crange[0])*(dd3-.04+.44*[0,1,1,0,0]) ), $
       10^( !y.crange[0] + $
            (!y.crange[1]-!y.crange[0])*(dd0+.05-.38*[0,0,1,1,0]) )
foo = where( limit eq '#' )
arrow, ea[foo], expnum[foo], ea[foo], expnum[foo]/yarr, hsize=!d.x_size/64, $
       hthick=2, solid=1, thick=5, /data
foo = where( limit eq 'EA' )
if keyword_set(rimp) then begin
  ;arrow, ea[foo], expnum[foo], ea[foo]/xarr, expnum[foo], hsize=!d.x_size/64, $
  ;       hthick=2, solid=1, thick=5, /data
  oplot, ea[foo], expnum[foo], thick=!p.thick*2, l=1
endif else begin
  arrow, ea[foo], expnum[foo], ea[foo]*xarr, expnum[foo], hsize=!d.x_size/64, $
         hthick=2, solid=1, thick=5, /data
endelse
foo = where( limit eq 'EA/#', count )
if count gt 0 then begin
  arrow, ea[foo], expnum[foo], ea[foo], expnum[foo]/yarr, hsize=!d.x_size/64, $
         hthick=2, solid=1, thick=5, /data
  arrow, ea[foo], expnum[foo], ea[foo]*xarr, expnum[foo], hsize=!d.x_size/64, $
         hthick=2, solid=1, thick=5, /data
endif
foo = where( limit ne '#', count )
if keyword_set(rimp) then begin
  for j=0,count-1 do oplot, ea[foo[[j,j]]], expnum[foo[[j,j]]]*[sqrt(2),1/sqrt(2)]
  for j=0,count-1 do oplot, ea[foo[[j,j]]]+[-1,1]*ea_sigma[foo[j]], expnum[foo[[j,j]]]
endif
foo = where( location eq 'C' )
solid_circles
oplot, ea[foo], expnum[foo], ps=8
oplot, [10^( !x.crange[0] + (!x.crange[1]-!x.crange[0])*dd3 )], $
       [10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*dd0 )], ps=8
xyouts, [10^( !x.crange[0] + (!x.crange[1]-!x.crange[0])*(dd3+dd4) )], $
        [10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*(dd0-dd2) )], $
        'C ring (non-equinox)', chars=1.5
foo = where( location eq 'Cq' )
oplot, ea[foo], expnum[foo], ps=8, co=ctwhite()
open_circles
oplot, ea[foo], expnum[foo], ps=8
oplot, [10^( !x.crange[0] + (!x.crange[1]-!x.crange[0])*dd3 )], $
       [10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*(dd0-dd1) )], ps=8
xyouts, [10^( !x.crange[0] + (!x.crange[1]-!x.crange[0])*(dd3+dd4) )], $
        [10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*(dd0-dd1-dd2) )], $
        'C ring (equinox)', chars=1.5
foo = where( location eq 'Bq' )
solid_triangles
oplot, ea[foo], expnum[foo], ps=8, co=ctwhite()
open_triangles
oplot, ea[foo], expnum[foo], ps=8
oplot, [10^( !x.crange[0] + (!x.crange[1]-!x.crange[0])*dd3 )], $
       [10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*(dd0-dd1*2) )], ps=8
xyouts, [10^( !x.crange[0] + (!x.crange[1]-!x.crange[0])*(dd3+dd4) )], $
        [10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*(dd0-dd1*2-dd2) )], $
        'B ring (equinox)', chars=1.5
foo = where( location eq 'A' )
solid_squares
oplot, ea[foo], expnum[foo], ps=8
oplot, [10^( !x.crange[0] + (!x.crange[1]-!x.crange[0])*dd3 )], $
       [10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*(dd0-dd1*3) )], ps=8
xyouts, [10^( !x.crange[0] + (!x.crange[1]-!x.crange[0])*(dd3+dd4) )], $
        [10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*(dd0-dd1*3-dd2) )], $
        'A ring (non-equinox)', chars=1.5
foo = where( location eq 'Aq' )
oplot, ea[foo], expnum[foo], ps=8, co=ctwhite()
open_squares
oplot, ea[foo], expnum[foo], ps=8
oplot, [10^( !x.crange[0] + (!x.crange[1]-!x.crange[0])*dd3 )], $
       [10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*(dd0-dd1*4) )], ps=8
xyouts, [10^( !x.crange[0] + (!x.crange[1]-!x.crange[0])*(dd3+dd4) )], $
        [10^( !y.crange[0] + (!y.crange[1]-!y.crange[0])*(dd0-dd1*4-dd2) )], $
        'A ring (equinox)', chars=1.5

foo = where( limit eq '' )
fit3 = svdfit( alog10(ea[foo]*mean(ea3/ea)), alog10(expnum[foo]), 2 )
fit4 = svdfit( alog10(ea[foo]), alog10(expnum[foo]), 2 )
oplot, 10^!x.crange, 10^poly( !x.crange, fit4 ), l=2

if keyword_set(dolzr) then clzr

end
