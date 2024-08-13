files = findfile('*scan*')
for j=0,n_elements(files)-1 do begin
  restore, files[j]
  openw, 1, files[j]+'.ascii'
  for k=0,n_elements(radi)-1 do begin
    printf, 1, radi[k], val[k]
  endfor
  close, 1
endfor

end
