pro reverse_elems, i, arg_peri, mean_anom, elems=elems

if keyword_set(elems) then begin
  elems_pl[*,2]=elems_pl[*,2]+180;!dpi
  elems_pl[*,4]=-elems_pl[*,4]+360;2*!dpi
  elems_pl[*,5]=-elems_pl[*,5]+360;2*!dpi
endif else begin
  i=i+180;!dpi
  arg_peri=-arg_peri+360;2*!dpi
  mean_anom=-mean_anom+360;2*!dpi
endelse

end