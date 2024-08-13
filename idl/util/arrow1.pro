; Draw arrows 

if keyword_set(arr_hsize) then begin
  _arr_hsize = arr_hsize * !d.x_size / 128 
endif else begin
  _arr_hsize = !d.x_size/128
endelse
if keyword_set(arr_thick) then _arr_thick=arr_thick else _arr_thick = 2
if not keyword_exists(arr_data) then arr_data = 1
arrow, x0, y0, x1, y1, hsize=_arr_hsize, hthick=1, /solid, $
       thick=_arr_thick, color=clr, data=arr_data

end
