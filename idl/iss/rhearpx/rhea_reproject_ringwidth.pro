if not keyword_exists(filenames) then restore, 'stretch.sav'
if not keyword_set(nf) then nf = n_elements(filenames)
if not keyword_exists(jjj) then jjj = -1
jjj = jjj + 1
image_name = filenames[jjj]
print, '----------'
print, strtrim(jjj,2)+' / '+strtrim(n_elements(filenames),2)+'   '+image_name
print, '----------'
@caviar
@rhea_rings
zra = zrarray1( et, nl, cam_params, cmat, polera, poledec, sc, /rhea, ll=ll )
rrpts = wher( abs(zra[1,*,*]) lt 1 )
rrfit = poly_fit( rrpts[1,*], rrpts[2,*], 1 )
rrang = atan( -rrfit[1] )*180/!pi

nn = 1440
v = [ [ring_coords[*,1]-mean(ring_coords[*,1])], $
      [ring_coords[*,0]-mean(ring_coords[*,0])], [replicate(0,nn)] ]
n = [ [replicate(0,1440)], [replicate(0,1440)], [replicate(1,1440)] ]
v2 = v_rotate( v, n, sin(-rrang*!pi/180), cos(-rrang*!pi/180) )
if not keyword_set(width) then width = dblarr(n_elements(filenames))
width[jjj] = max(abs( v2[*,1] ))
