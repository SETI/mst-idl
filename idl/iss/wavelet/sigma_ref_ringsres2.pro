blankplot = 1
.run sigma_ref
cd, '$DATA/images/077/RDHRCOMP'
restore, 'stretch.sav'
radius077 = reform(_keywords.ringplane_aimpoint_radius)
restore, 'ring_rads_index.sav'
sigma077 = radscan_sigma
solid_circles
oplot, tkm(radius077), sigma077, ps=8
