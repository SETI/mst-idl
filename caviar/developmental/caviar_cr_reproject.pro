pro caviar_cr_reproject,im,rawim,radius_in,radius_out,cr_start_lon,cr_stop_lon,et,polera,poledec,cam_params,nl,$
cmat,vobs_planet,cr_epoch_str,meanmot

cspice_str2et,cr_epoch_str,et_cr_epoch

start_lon=cr_start_lon+((et-et_cr_epoch)*meanmot/86400.0d0)
stop_lon=cr_stop_lon+((et-et_cr_epoch)*meanmot/86400.0d0)

start_lon=start_lon+(fix(start_lon/360.0d0)*(-360.0d0))
stop_lon=stop_lon+(fix(stop_lon/360.0d0)*(-360.0d0))

if start_lon lt 0.0d0 then start_lon=start_lon+360.0d0
if stop_lon lt 0.0d0 then stop_lon=stop_lon+360.0d0


wset=1
get_ring,et,[radius_in,radius_out],start_lon,stop_lon,polera,poledec,3600,reproj_region,699L
image_coords,reproj_region,cmat,vobs_planet,cam_params,nl,reproj_region_coords
reproj_region_symbol=3
reproj_region_symbol_size=0.5
plots,round(reproj_region_coords[*,1]),(nl-1)-round(reproj_region_coords[*,0]),psym=reproj_region_symbol,$
symsize=reproj_region_symbol_size,color=make_array(n_elements(reproj_region[*,0]),value=green()),/device
plots,round([reproj_region_coords[0,1],reproj_region_coords[3600,1]]),(nl-1)-round([reproj_region_coords[0,0],$
reproj_region_coords[3600,0]]),linestyle=0,color=green(),/device
plots,round([reproj_region_coords[3599,1],reproj_region_coords[7199,1]]),(nl-1)-round([reproj_region_coords[3599,0],$
reproj_region_coords[7199,0]]),linestyle=0,color=green(),/device

caviar_reproj,polera,poledec,699L,im,rawim,et,cmat,radius_in,radius_out,$
start_lon,stop_lon,cam_params,nl


return
end