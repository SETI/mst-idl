basedir = '/home/borogove/iss/images/'
; Note 029/AZSCNLOPH (F Ring), 057/AZSCAN (Cassini Div), 060/AZSCAN (C Ring)
dirs = [ '007/AZSCNLOPH', '013/AZSCNHIPH001', '035/AZSCNLOPH', '043/AZSCAN', '064/AZSCAN' ]
suff = [ ' (Encke/Keeler)', ' (Encke)', ' (Encke)', ' (Keeler)', ' (Encke)' ]
ndirs = n_elements(dirs)
!p.multi = [0,2,3]
!p.charsize = 1.5
for k=0,ndirs-1 do begin
  restore, basedir+dirs[k]+'/stretch.sav'
  foo = where( strmid(filenames,0,1) eq 'N' )
  plot, _keywords[foo].ringplane_aimpoint_longitudinal_scale_km, /xs, $
        xtit='Image #', ytit='Pixel scale', tit=dirs[k]+suff[k], yr=[0,10]
  oplot, _keywords[foo].ringplane_aimpoint_radial_scale, l=5
  print, dirs[k]+': Avg Min Rad '+$
         string(mean(_keywords[foo].ringplane_minimum_radius),fo='(F9.2)')+$
         ', Avg Max Rad '+$
         string(mean(_keywords[foo].ringplane_maximum_radius),fo='(F9.2)')
endfor

end
