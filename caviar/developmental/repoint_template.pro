PRO repoint_template, files, mnrad=mnrad, mxrad=mxrad, thres=thres, smlen=smlen

if not keyword_set(mnrad) then mnrad=90e3
if not keyword_set(mxrad) then mxrad=115e3
temp_mnrad=mnrad
temp_mxrad=mxrad
if keyword_set(thres) then thresx=thres

restore, '/home/sordes2/iss/images/davi3789/SPKMVLFLP_081/W1597993945_1_cal.scan1' 
temp_radi=radi
temp_val=val
nf=n_elements(files)
noplot=1
nostars=1
basiconly=1
for i=0,nf-1 do begin
image_name=files(i)
@caviar
if keyword_set(thresx) then thresxx=thresx
templatepoint, files(i), temp_radi, temp_val, x_move, y_move, mnrad=temp_mnrad, mxrad=temp_mxrad, thres=thresxx, smlen=smlen
print, x_move, y_move
delvar, thresxx
@move_bypixel
overwrite=1
@save_point
if keyword_set(thresx) then thresxx=thresx
templatepoint, files(i), temp_radi, temp_val, x_move, y_move, mnrad=temp_mnrad, mxrad=temp_mxrad, thres=thresxx, smlen=smlen
print, x_move, y_move
delvar, thresxx
@move_bypixel
overwrite=1
@save_point
if keyword_set(thresx) then thresxx=thresx
templatepoint, files(i), temp_radi, temp_val, x_move, y_move, mnrad=temp_mnrad, mxrad=temp_mxrad, thres=thresxx, smlen=smlen
print, x_move, y_move
delvar, thresxx
@move_bypixel
overwrite=1
@save_point
end


end
