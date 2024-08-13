kuniolist = [ 'LIST_1509.txt', 'LIST_1511_cl1cl2.txt', 'LIST_1512_cl1mt2.txt' ]
nklists = n_elements(kuniolist)
for j=0,nklists-1 do begin
  openr, 1, kuniolist[j]
  aa = ''
  spawn, 'wc '+kuniolist[j], num
  num = fix(strmid( num[0], 0, 10 ))
  kfiles = strarr(num)
  for k=0,num-1 do begin
    readf, 1, aa
    kfiles[k] = strmid(aa,0,11)
  endfor
  if not eof(1) then stop
  ; kfiles1509 = kfiles
  foo = execute( 'kfiles15'+(['09','11','12'])[j]+' = kfiles' )
  close, 1
endfor

restore, '017_ftrack/spreadsheet.sav'
files_017_ftrack = [ [data[0,where( data[4,*] eq 'CB2' )]], $
                     [data[0,where( data[4,*] eq 'MT2' )]] ]
files_017_ftrack = strmid( files_017_ftrack, 0, 11 )
print, where( files_017_ftrack ne kfiles1509 )
for k=0,n_elements(kfiles1509)-1 do begin
  print, where( strmid(data[0,*],0,11) eq 'N'+strmid(kfiles1509[j],1,10) )
endfor

restore, '018_firmap/spreadsheet.sav'
files_018_firmap = data[0,*]
files_018_firmap = strmid( files_018_firmap, 0, 11 )
print, where( files_018_firmap ne kfiles1511 )
for k=0,n_elements(kfiles1511)-1 do begin
  print, where( strmid(data[0,*],0,11) eq 'N'+strmid(kfiles1511[j],1,10) )
endfor

restore, '018_1x2plrnch/spreadsheet.sav'
files_018_1x2plrnch = [ data[0,where( data[4,*] eq 'CL1' and $
                                      data[5,*] eq 'MT2' )] ]
files_018_1x2plrnch = strmid( files_018_1x2plrnch, 0, 11 )
restore, '018_1x2plrnck/spreadsheet.sav'
files_018_1x2plrnck = [ data[0,where( data[4,*] eq 'CL1' and $
                                      data[5,*] eq 'MT2' )] ]
files_018_1x2plrnck = strmid( files_018_1x2plrnck, 0, 11 )
files_018_1x2plrnc = [ [files_018_1x2plrnch], [files_018_1x2plrnck] ]
print, where( files_018_1x2plrnch ne kfiles1512 )

end
