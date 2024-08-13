pro read_radscan, filenames

if not keyword_set(filenames) then filenames=findfile('*ascii')
nf=n_elements(filenames)
for j=0,nf-1 do begin

  period = rstrpos( filenames[j], '.' )
  newfile = strmid( filenames[j], 0, period )
  close, 1
  openr, 1, filenames[j]
  a = ''
  radi = 0.
  val = 0.
  while not eof(1) do begin
    readf, 1, a
    b = strsplit( a, ' ' )
    radi = [ radi, strmid( a, b[0], b[1]-b[0] ) ]
    val = [ val, strmid( a, b[1], strlen(a) ) ]
  endwhile
  radi = radi[1:n_elements(radi)-1]
  val = val[1:n_elements(val)-1]

  save, radi, val, filename=newfile

endfor

end
