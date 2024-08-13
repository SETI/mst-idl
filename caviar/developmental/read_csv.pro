function read_csv, filename, silent=silent

; Reads a comma-delimited text file.  All lines must have the same number of 
; line separators. (Note that we use \ instead of a comma for the line separator
; because sometimes the Description field contains commas.)
; Returns output, a string array where x is the number of columns and y is the 
; number of lines.  

openr, 1, filename
a = ''

character = '\'
readf, 1, a
comma = strpos( a, character )
ncom = 0
while comma ne -1 do begin
  comma = strpos( a, character, comma+1 )
  ncom = ncom + 1
endwhile
ncol = ncom + 1
nrow = 0
output = strarr(ncol)

while not eof(1) do begin
  if nrow ne 0 then readf, 1, a
  output = [ [output], [strarr(ncol)] ]
  nrow = nrow + 1
  comma = -1
  for j=0,ncol-1 do begin
    comma1 = strpos( a, character, comma+1 )
    if comma1 eq -1 then comma1 = strlen(a)
    output[j,nrow] = strmid( a, comma+1, comma1-comma-1 )
    comma = comma1
  endfor
endwhile
output = output[*,1:nrow]
close, 1

if not keyword_set(silent) then begin
  print, 'The usual things to do next:'
  print, 'legend = data[*,0]'
  print, 'data = data[*,1:'+strtrim(nrow-1,2)+']'
  print, 'exposure_duration = long(data[3,*])'
endif

return, output

end
