; Get reproj_region_coords by inserting a "stop" line in kids.pro

rrc = reproj_region_coords
rrc1=reproj_region_coords[0:3599,*]
rrc2=reproj_region_coords[3600:7199,*]
rrclx = [ min(rrc[*,1]), max(rrc[*,1]) ]
rrcly = [ min(rrc[*,0]), max(rrc[*,0]) ]

xs=1900.
ys=1000.
aspect = min([ xs/(rrclx[1]-rrclx[0]), ys/(rrcly[1]-rrcly[0]) ])
window, xs=xs, ys=ys
tv, bytarr(xs,ys)+255
mnx = min(rrc[*,1])
mny = max(rrc[*,0])
!p.thick=3
plots, /device, (rrc1[*,1]-mnx)*aspect, (mny-rrc1[*,0])*aspect, color=green()
plots, /device, (rrc2[*,1]-mnx)*aspect, (mny-rrc2[*,0])*aspect, color=green()
plots, /device, ([rrc1[0,1],rrc2[0,1]]-mnx)*aspect, (mny-[rrc1[0,0],rrc2[0,0]])*aspect, color=green()
plots, /device, ([rrc1[3599,1],rrc2[3599,1]]-mnx)*aspect, (mny-[rrc1[3599,0],rrc2[3599,0]])*aspect, color=green()

end
