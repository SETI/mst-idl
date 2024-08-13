; This development version, written 24 July 2006, was used to test the method.
; The purpose was to show that moving the image pointing in the radial direction
; results in a pure vertical translation of the edgefit, and that moving the 
; pointing in the orbital direction results in a pure twisting of the edgefit.  
; To do this, set dr and dl recursively, as seen in
; 007/AZSCNLOPH/bootstrap_redge.ps and 008/LPHRLFMOV/bootstrap_redge.ps

if not keyword_set(savefile) then savefile='N1495091875_1_cal.edge1'
;if not keyword_set(savefile) then savefile='N1493713875_1_cal.edge1'

redge1 = linsam_to_redge(cmat,savefile=savefile,redge1_sigma=redge1_sigma)
;plot_redge, redge1, redge1_sigma
;bryr=[136.48,136.513]
tit='!MDrad = '+strtrim(dr,2)+', !MDlon = '+strtrim(dl,2)
plot, redge1[0,*], tkm(redge1[1,*]), /xs, xr=brxr, /ys, yr=bryr, tit=tit

if not keyword_set(arr_len) then arr_len = 1
noplot = 1
@draw_arrows

if not keyword_exists(dr) then dr = 1
if not keyword_exists(dl) then dl = 0

;Use radial and orbital
x_move = (arr_sat_coords[0,1]-arr_sat_coords[1,1])*dr + $
         (arr_orb_coords[1,1]-arr_orb_coords[0,1])*dl
y_move = (arr_sat_coords[0,0]-arr_sat_coords[1,0])*dr + $
         (arr_orb_coords[1,0]-arr_orb_coords[0,0])*dl
;;Use radial and perpendicular-to-radial
;x_move = (arr_sat_coords[0,1]-arr_sat_coords[1,1])*dr + $
;         (nl-1-arr_sat_coords[1,0]-arr_sat_coords[0,0])*dl
;y_move = (arr_sat_coords[0,0]-arr_sat_coords[1,0])*dr + $
;         (arr_sat_coords[1,1]-arr_sat_coords[0,1])*dl
@move_bypixel

redge2 = linsam_to_redge(cmat,savefile=savefile,redge1_sigma=redge2_sigma)
;plot_redge, redge2, redge2_sigma, /oplot, color=green()
oplot, redge2[0,*], tkm(redge2[1,*]), co=green()

end
