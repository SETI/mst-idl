openr, 1, 'kernels.ker'
aa = ''
readf, 1, aa
while not eof(1) do begin
  kernelnamepos = strpos( aa, '/home/sauron2' )
  if kernelnamepos ne -1 then begin
    kernelpath = strmid( aa, kernelnamepos, strlen(aa)-kernelnamepos-2 )
    lastslash = rstrpos( kernelpath, '/' )
    ;lastslash2 = rstrpos( strmid(kernelpath,0,lastslash), '/' )
    if strpos( kernelpath, '/ck/' ) ne -1 or $
       strpos( kernelpath, '/spk/' ) ne -1 then begin
      dest = strmid(kernelpath,0,lastslash)+'2'
      if not keyword_set(findfile(dest)) then stop, dest
      com = 'mv '+kernelpath+' '+dest
      print, com
      spawn, com
    endif 
  endif 
  readf, 1, aa
endwhile
close, 1

end
