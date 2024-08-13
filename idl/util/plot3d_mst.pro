function plot3d_mst, coords3d, xang=_xang, debug=debug, degrees=degrees, radians=radians

sz = size(coords3d)
if sz[0] ne 2 then stop, 'coords3d must have 2 dimensions.'
if sz[2] ne 3 then stop, 'coords3d must be an nx3 array.'
nn = sz[1]
case sz[3] of
  1: coords2d = bytarr(nn,2)
  2: coords2d = intarr(nn,2)
  3: coords2d = lonarr(nn,2)
  4: coords2d = fltarr(nn,2)
  5: coords2d = dblarr(nn,2)
  7: stop, 'coord3d cannot be a string'
  else: stop
endcase

if keyword_set(_xang) then xang=_xang else xang=45
if abs(xang) lt 2*!pi and not keyword_set(radians) and not keyword_set(degrees) then stop, 'Is xang in radians? It should be in degrees.'
if not keyword_set(radians) then xang = xang*!pi/180
coords2d[*,0] = coords3d[*,1] - coords3d[*,0]*cos(xang)*0.7
coords2d[*,1] = coords3d[*,2] - coords3d[*,0]*sin(xang)*0.7
if keyword_set(debug) then stop
return, coords2d

end
