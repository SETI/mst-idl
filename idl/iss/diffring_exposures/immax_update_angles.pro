restore, 'stretch.sav'
restore, 'immax.sav'
nim = n_elements(immax[0,*])
if not keyword_set(ims) then ims = lindgen(nim)
phase = dblarr(nim)
emission = dblarr(nim)
noplot = 1
for jjj=0,nim-1 do begin
  image_name=filenames[ims[jjj]]
  @caviar
  if immax[0,jjj] eq -1 then aimpoint = 0 else aimpoint = immax[*,jjj]
  @calculate_keywords
  phase[jjj] = keywords.ringplane_aimpoint_phase_angle
  emission[jjj] = keywords.ringplane_aimpoint_emission_angle
endfor
@save_immax

end
