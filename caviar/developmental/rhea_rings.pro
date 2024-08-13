nn = 4;6
rmax = 4
ring_rads = 764.0d0 * ( (findgen(nn)/( nn - 1 )*( rmax - 1 )) + 1 )
ring_rads_legend = string(ring_rads/764,fo='(F3.1)')+' R_Rhea'
startlon = 0
stoplon = 360
get_ring,et,ring_rads,startlon,stoplon,polera,poledec,ring_npoints,ring,605L,light_time=light_time
image_coords,ring,cmat,vobs_planet,cam_params,nl,ring_coords
xrstart=round(ring_coords[*,1])
yrstart=round(ring_coords[*,0])
xr=xrstart
yr=yrstart
ring_symbol = -3
ring_num = 0
;noplot = 1
@plot_rings
;noplot = 0
;
;pnum = (where( plan_names eq 'Rhea' ))[0]
;
;xrstart=round(ring_coords[*,1])
;yrstart=round(ring_coords[*,0])
;xr=xrstart
;yr=yrstart
;@plot_rings
