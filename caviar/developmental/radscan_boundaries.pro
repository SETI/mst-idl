spawn, 'pwd', pwd
pwd = pwd[0]
lastslash = rstrpos( pwd, '/' )
psname = 'radscan_boundaries_'+strmid(pwd,lastslash-3,3)+$
         '_'+strmid(pwd,lastslash+1,100)
if keyword_set(dolzr) then begin
  if keyword_set(findfile(psname+'.ps')) then stop, psname+' exists'
  lzr, psname
  @plot_prepare
  plot_color
endif
if not keyword_set(max_radi) then begin
  restore, 'stretch.sav'
;  restore, 'ring_rads_index.sav'
  foo = where( strmid(filenames,0,1) eq 'N', nfiles )
  min_radi = dblarr(nfiles)
  max_radi = dblarr(nfiles)
  for jj=0,nfiles-1 do begin
    image_name = filenames[jj]
    @restore_radscan1
    min_radi[jj] = min(radi)
    max_radi[jj] = max(radi)
  endfor
  dradi = max_radi[0:nfiles-2] - min_radi[1:nfiles-1]
endif

note = 'Created via .run radscan_boundaries'
savefile = 'max_radi.sav'
if not keyword_set(findfile(savefile)) then begin
  foo = where( strmid(filenames,0,1) eq 'N', nfiles )
  save, filenames, foo, max_radi, min_radi, nfiles, note, filename=savefile
endif

solid_circles
plot, findgen(nfiles-2)+0.5, dradi, xtit='Image #', ytit='Overlap (km)', $
      ps=-8, /xs, xr=[0,nfiles-1], ys=8, xma=[10,10]
oplot, !x.crange, [0,0], l=1
axis, yaxis=1, yr=[tkm(min(min_radi)),tkm(max(max_radi))], $
      /ys, /save, ytit='Radius'+tkmtit(), co=ctred()
oplot, tkm(rebin([ [min_radi], [max_radi] ],nfiles,1)), ps=-8, co=ctred(), $
       noclip=1
if keyword_set(dolzr) then clzr

end
