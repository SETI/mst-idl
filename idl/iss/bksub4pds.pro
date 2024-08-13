if not keyword_set(image_name) then image_name = 'N1884110977_1_cal.IMG'
second_underscore = strpos( image_name, '_', strpos(image_name,'_')+1 )
filestem = strmid(image_name,0,second_underscore)

rawim = read_vicar(image_name)
restore, filestem+'_cal.bksub'
write_vicar, filestem+'_bksub.IMG', bksub_img
write_vicar, filestem+'_radial.IMG', rawim - bksub_img

stop

noplot = 1
restore, filestem+'_cal.scan1'
openw, 1, filestem+'_radscan.txt'
printf, 1, 'Descriptor:  '+radscan_descrip
printf, 1, ''
printf, 1, 'C matrix used to make this scan:'
fo = '(F20.10,F20.10,F20.10)'
for j=0,2 do printf, 1, fo=fo, radscan_cmat[*,j]
printf, 1, ''
printf, 1, 'I suppose, in order for the C matrix to be meaningful, I should list the trajectory kernels used.'
printf, 1, ''
printf, 1, 'Four columns.  The first column is Radius, in km.  The other three columns are the mean, standard deviation, and number of the I/F values of pixels within the radial bin.'
printf, 1, ''
printf, 1, 'I''ll need to subtract a constant from the radius values, in order to fix the pointing.  I''ll want to quote that constant here.'
close, 1

openw, 1, filestem+'_radscan.dat'
fo = '(E20.10,E20.10,E20.10,E20.10)'
for j=0,n_elements(radi)-1 do begin
  printf, 1, fo=fo, radi[j], val[j], errbar[j], radscan_np[j]
endfor
close, 1

end
