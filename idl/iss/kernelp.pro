if not keyword_set(kernellist) then begin
  print, 'Enter kernel paths into kernellist.'
  retall
endif
nk = n_elements(kernellist)
for j=0,nk-1 do begin
  spawn, 'toxfr '+kernellist[j]+' temp', output
  for k=0,n_elements(output)-1 do print, output[k]
  if n_elements(output) ne 2 then stop
  spawn, '_rm '+kernellist[j]
  spawn, 'tobin temp '+kernellist[j], output
  for k=0,n_elements(output)-1 do print, output[k]
  if n_elements(output) ne 2 then stop
  spawn, '_rm temp'
endfor

end
