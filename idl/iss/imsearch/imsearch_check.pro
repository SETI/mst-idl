encke = 1
.run prop_table
restore, '../imsearch/search4images_prop_imtable_110106.sav'
foo1 = where( match eq 'Bleriot' and _radlon_sigma[0,*] lt 50, count1 )
foo2 = lonarr(count1)
for j=0,count1-1 do foo2[j] = where( strmid(_image_name[foo1[j]],0,11) eq images )
foo3 = where( foo2 eq -1, count3 )
if count3 ne 0 then stop

!p.multi = [0,2,2]
solid_diamonds
;plot_nosci, __et[foo1]/86400/365.25+2000, _radlon[0,foo1], ps=8, /ynozero, $
;            xtit='Year', ytit='Radius (km)'
;oploterr, __et[foo1]/86400/365.25+2000, _radlon[0,foo1], _radlon_sigma[0,foo1]
plot, __et[foo1]/86400/365.25+2000, lon[foo2]-_radlon[1,foo1], ps=8, /ynozero, $
      xtit='Year', ytit='Longitude Residual (!Uo!N)'
oploterr, __et[foo1]/86400/365.25+2000, lon[foo2]-_radlon[1,foo1], _radlon_sigma[1,foo1]
dpx = [ planet_coords[0,foo2]-_xy[0,foo1], planet_coords[1,foo2]-_xy[1,foo1] ]
plot, dpx[0,*], dpx[1,*], ps=8, xtit='x', ytit='y'
plot, __et[foo1]/86400/365.25+2000, sqrt(dpx[0,*]^2+dpx[1,*]^2), xtit='Year', ytit='Pixel Residual', ps=8
