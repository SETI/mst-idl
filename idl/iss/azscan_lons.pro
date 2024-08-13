; For movies:  Inner edge 139378.09, outer edge 136505.0

dir = '/home/borogove/iss/images/'
obs = [ '007/LPHRLFMOV', '007/AZSCNLOPH', '008/LPHRLFMOV' ]
nobs = n_elements(obs)
savefile = 'azscan_lons.sav'
if keyword_set(findfile(savefile)) then restore, savefile else begin

  imlon = 0.0d0
  daphlon = 0.0d0
  np = 0l
  for k=0,nobs-1 do begin
    restore, dir + obs[k] + '/stretch.sav'
    restore, dir + obs[k] + '/et.sav'
    imlon = [ imlon, reform(_keywords.ringplane_aimpoint_longitude) ]
    np = [ np, np[n_elements(np)-1]+n_elements(filenames) ]
    @get_sat_prepare
    sat = 635l
    for j=0,n_elements(filenames)-1 do begin
      et = _et[j]
      @get_sat_coords
      daphlon = [ daphlon, sat_polar[1] ]
    endfor 
  endfor
  imlon = clip(imlon)
  daphlon = clip(daphlon)

  save, imlon, daphlon, np, filename='azscan_lons.sav'

endelse

daphim = [ 109, 183, 121 ]
plot, [-190,235], [-22,120], /xs, /ys, /nodata, $
      xtit='Longitude relative to Daphnis (!Uo!N)', $
      ytit='Longitude relative to image containing Daphnis (!Uo!N)'
clr = [ ctred(), ctblue(), ctgreen() ]
for k=0,nobs-1 do begin
  oplot, unwrap_phase( imlon[np[k]:np[k+1]-1]-daphlon[np[k]:np[k+1]-1] ), $
         fix_angles( imlon[np[k]:np[k+1]-1]-imlon[np[k]+daphim[k]], /deg ), $
         co=clr[k]
  xyouts, -150, 108-k*5, obs[k], co=clr[k]
endfor

end
