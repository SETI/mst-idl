lzr, 'image_coverage_plot', /port
@plot_prepare
plot_color

dirs = [ '007/AZSCNLOPH', '013/AZSCNHIPH001', '035/AZSCNLOPH', '043/AZSCAN', $
         '064/AZSCAN', '070/PAZSCN001', '081/AZSCANDRK', '087/FMOVIE', $
         '090/SHRTMOV' ]
;'092/FRINGHRDF', '096/FMOVIE'
basedir = '$DATA/images/'
xtit = 'Image #'
ytit = 'Radial Scale (solid)!CLongitudinal Scale  (dashed)'
!p.multi = [0,3,8]
!p.charsize = 1

k = -1

k = k + 1
print, strmid(k,2) + ' / ' + strmid(n_elements(dirs)-1,2)
cd, basedir + dirs[k]
.run image_outlines
.run image_coverage
restore, 'stretch.sav'
foo = where( strmid(filenames,0,1) eq 'N' )
yr = [ min([_keywords[foo].(10),_keywords[foo].(11)]), $
       max([_keywords[foo].(10),_keywords[foo].(11)]) ]
plot, _keywords[foo].(10), /xs, /ys, yr=yr, xtit=xtit, ytit=ytit
oplot, _keywords[foo].(11), l=2
