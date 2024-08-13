PRO get_streaks_u, label, cmat,pmat, nacmat,stars, pixscale, x_streaks, y_streaks
;modified 062713 in order to allow the system to properly use a shifted pointing


image_data_streak_u,label,ets,epoch,exposure,cam_name,pmats,nl,found
ns=n_elements(stars(*,0))
x_streaks=fltarr(9,ns)
y_streaks=fltarr(9,ns)
for i=0,8 do begin 
	et=reform(ets(i))
	cmatx=cmat+nacmat ## reform(pmats(i,*,*)-pmat)
	cspice_spkez,-32L,ets(i),'J2000','NONE',0L,state,lighttime
	vobs=state[3:5]
	image_coords_u,stars,cmatx,vobs,pixscale,nl,coords
	x_streaks(i,*)=(coords[*,1])
	y_streaks(i,*)=(coords[*,0])
	end
end
