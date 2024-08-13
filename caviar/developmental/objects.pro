pro objects,image_name,unknown_objects,cmat

length_name_string=strlen(image_name)

name_string=image_name

if length_name_string ne 17 then name_string=strmid(image_name,length_name_string-17,17)

name_string=strmid(name_string,0,13)

output_name=name_string+'.uio'

save_path=' '

print,''
print,'Input path to directory to save objects file. RETURN saves file in current directory'
print,'       (Path must include the trailing forward slash, /, at the end)          '
print,' '
read,save_path
print,' '

save_path=strcompress(save_path,/remove_all)

cspice_m2q,cmat,cmat_quat

openw,lun,save_path+output_name,/get_lun
out=float(transpose(unknown_objects))
printf,lun,cmat_quat
printf,lun,' '
printf,lun,'       line         sample       flux       radius         long'
printf,lun,' '
printf,lun,out
free_lun,lun

return
end