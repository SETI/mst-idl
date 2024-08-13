function smooth_clip, array, width, missing=missing, nan=nan, xin=xin, xout=xout

if n_params() eq 0 then begin
  print, 'SMOOTH_CLIP:  Use this function as you would use the SMOOTH function.'
  print, 'The difference is that the regions within one smoothing length of the edges will'
  print, 'be clipped away, rather than smoothed imperfectly or left untouched (the only'
  print, 'options offered by SMOOTH).'
  print, 'Set keywords xin= and xout= to perform the same clipping on an associated x-coordinate.'
  return, -1
endif

result = smooth( array, width, missing=missing, nan=nan )
result = result[width:n_elements(result)-width-1]
if keyword_set(xin) then begin
  xout = xin[width:n_elements(xin)-width-1]
endif

return, result

end
