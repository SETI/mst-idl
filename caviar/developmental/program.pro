offs=FINDFILE('*.offset')
for j=0,n_elements(offs)-1 do begin
	if keyword_set(rpk_kernel) then delvar, rpk_kernel
	restore, offs[j]
	if keyword_set(point_descrip) then stop, point_descrip   

	; I don't think this conditional will be true in any case, but it's good to check. 

	point_descrip = 'stars'
	save, cmat, point_descrip, point_method, rpk_kernel, filename=offs[j]

endfor
end
