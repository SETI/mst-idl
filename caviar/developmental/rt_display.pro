pro rt_display, filenames, nostars=nostars, noplot=noplot, nonstop=nonstop

for im_num = 0, n_elements(filenames) do begin
	image_name = filenames[im_num]
	@caviar
	if(nonstop ne 1) then begin
		print, 'Enter ".c" to continue'
		stop
	endif
endfor

end