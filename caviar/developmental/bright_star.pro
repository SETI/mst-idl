pro bright_star, stars, brightstar, nostars=nostars

; Bright stars are not generally in the Tycho-2 or UCAC catalogues. 
; So add them manually. 

n_stars=n_elements(stars[*,0])
stars = [ stars, [[0],[0],[0],[0]] ]

; To update bright_star.sav with new stars, look here:
; http://pds-rings.seti.org/tools/starlist_sat.txt
restore, getenv('DATA2')+'/caviar/developmental/bright_star.sav'
nbs = n_elements(bsname)

if brightstar gt 0 then begin
  ; If directory name is a star name, then use that star automatically.
  ; Set brightstar = -1 to override
  spawn, 'pwd', pwd
  lastslash = rstrpos( pwd, '/' )
  foo = where( strmid( pwd[0], lastslash+1, strlen(bsname1) ) eq bsname1, c1 )
  if c1 eq 1 then begin
    reply = foo[0]+1
    brightstar = foo[0]+1
  endif else begin
    foo = where( strmid( pwd[0], lastslash+3, strlen(bsname1) ) eq bsname1, c2 )
    if c2 eq 1 then begin
      reply = foo[0]+1
      brightstar = foo[0]+1
    endif
  endelse
endif
if not keyword_set(reply) then reply = ''
while reply eq '' do begin
  print, 'Select bright star to display for this image:'
  for j=0,nbs-1 do print, strtrim(j+1,2)+':  '+bsname[j]
  read, reply
  if reply eq 'q' then retall
  if reply gt 0 and reply le nbs then brightstar = reply else reply = ''
endwhile
; Convert RA from hours to degrees
ra = ra * 360 / 24
; Convert RA, dec, and mag to Caviar long integer format
ras = 3600.0d3 * ra[brightstar-1]
decs = 3600.0d3 * dec[brightstar-1]
mags = 100 * mag[brightstar-1]
stars[n_stars,*] = [ [0], [round(ras)], [round(decs)], [round(mags)] ]
print, bsname[brightstar-1]+' added to star catalogue for this image.'

if keyword_set(nostars) then nostars = 0

end
