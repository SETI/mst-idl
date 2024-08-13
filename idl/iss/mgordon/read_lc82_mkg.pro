savefile = 'lc82_mkg.sav'

if keyword_set(findfile(savefile)) then restore, savefile else begin
  files = findfile('L*csv')
  nfiles = n_elements(files)
  moon = 0l
  ll = 0l
  mm = 0l
  order = 0l
  rres = 0.0
  torque = 0.0
  x_nl = 0.0
  x_max = 0.0
  param = [ 'moon', 'll', 'mm', 'order', 'rres', 'torque', 'x_nl', 'x_max' ]
  nparam = n_elements(param)
  for j=0,nfiles-1 do begin
    print, 'Opening '+files[j]
    openr, 1, files[j]
    aa = ''
    while not eof(1) do begin
      readf, 1, aa
      commas = [ strsplit( aa, ',' ), strlen(aa)+1 ]
      for k=0,nparam-1 do begin
        ;moon = [ moon, strmid( aa, commas[0], commas[1]-commas[0]-1 ) ]
        foo = execute( param[k]+' = [ '+param[k]+', strmid( aa, commas['+$
                       strtrim(k,2)+'], commas['+strtrim(k+1,2)+']-commas['+$
                       strtrim(k,2)+']-1 ) ]' )
      endfor
    endwhile
    close, 1
  endfor
  savecom = 'save, '
  for k=0,nparam-1 do begin
    ;moon = clip(moon)
    foo = execute( param[k]+' = clip('+param[k]+')' )
    if k eq 0 then begin
      ;nn = n_elements(moon)
      foo = execute( 'nn = n_elements('+param[k]+')' )
    endif else begin
      ;if n_elements(moon) ne nn then stop
      foo = execute( 'if n_elements('+param[k]+') ne nn then stop' )
    endelse 
    savecom = savecom + param[k] + ', '
  endfor
  savecom = savecom + 'nn, filename=savefile'
  ; save, [params], nn, filename=savefile
  foo = execute( savecom )
endelse

end
