; 110/FMOVIE
restore, 'stretch.sav'
jjj = 60
restore, strmid(filenames[jjj],0,17)+'.edge1'
redge[1,*] = median(reform(redge[1,*]),10) 
if keyword_set(dolzr) then begin
  lzr, 'ethanexample', /half
  @plot_prepare
  !p.multi = [0,1,2]
  !p.charsize = 1.5
endif
plot_nosci, redge[0,*], redge[1,*], /ynoz, /xs, $
            xtit='Longitude (deg)', ytit='Radius (km)'
if keyword_set(dolzr) then clzr

end
