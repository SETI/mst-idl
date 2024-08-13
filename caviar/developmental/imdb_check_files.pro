if not keyword_set(csv) then csv = '~/Desktop/images_since_210.csv'
openr, 1, csv
a = ''
readf, 1, a
tabloc = strpos( a, 'Image_Mid_Time' ) - 1
tab = strmid( a, tabloc, 1 )
lastcsvdir = ''
while not eof(1) do begin
  readf, 1, a
  csvfilename = strmid( a, 0, 11 )
  csvdir = strmid( a, (strsplit(a,tab))[2], 1000 )
  if csvdir ne lastcsvdir then begin
    if keyword_set(dir) then begin
      print, dir + ': ' + strtrim(nfound,2) + ' of ' + $
             strtrim(nfiles,2) + ' found.'
    endif 
    nouse = 0
    dir = ''
    lastcsvdir = csvdir
    while dir eq '' do begin
      print, 'New observation name: '+csvdir
      print, 'Type directory name, or [s]kip'
      read, dir
      if dir eq 'q' then retall else if dir eq 's' then nouse = 1 else begin
        if not keyword_set(findfile(dir)) then dir = ''
      endelse 
    endwhile 
    nfiles = 0
    nfound = 0
  endif 
  if not keyword_set(nouse) then begin
    nfiles = nfiles + 1
    foo = keyword_set(findfile( dir + '/' + csvfilename + '*' ))
    if foo then nfound = nfound + 1 else begin
      print, csvfilename + ' not found'
    endelse 
  endif 
endwhile

close, 1

end
