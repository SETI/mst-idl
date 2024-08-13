pro findfile2, filename

; Looks through all files accessible through the PATH environment variable,
; and prints the pathnames of all files matching the input filename.

path_to_array,dirlist

for i=0,n_elements(dirlist)-1 do begin
  spawn, 'ls -1 '+dirlist[i], f
  f1=strmid(f,0,strlen(filename))
  g=where(f1 eq filename)
  if g[0] ne -1 then print,dirlist(i)+'/'+f[g]
endfor

end
