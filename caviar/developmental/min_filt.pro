	pro min_filt,im1,im2,im3

	ns=n_elements(im1)
	n=sqrt(ns)

	im3=im2

	for j=0,n-1 do begin
	for k=0,n-1 do begin

	if im1[j,k] le im2[j,k] then im3[j,k]=im1[j,k]

	endfor
	endfor

	return
	end