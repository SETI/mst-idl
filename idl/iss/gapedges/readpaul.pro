basedir = '/home/borogove/iss/images/ptorrey/'
dir = '035/AZSCNLOPH'
savedir = '035_azscnloph'
if keyword_set(inner) then begin
  filename = basedir + dir + '/EnckeGapRev035inner.dat'
  savefile = savedir + '_inner.sav'
endif else begin
  filename = basedir + dir + '/EnckeGapRev035outer.dat'
  savefile = savedir + '_outer.sav'
endelse

openr, 1, filename
aa = ''
nn = 0l
while not eof(1) do begin
  readf, 1, aa
  nn = nn + 1
endwhile
close, 1
lon = fltarr(nn)
rad = fltarr(nn)
imnum = intarr(nn)
openr, 1, filename
for j=0l,nn-1 do begin
  if j mod 1000 eq 0 then print, strtrim(j,2)+' / '+strtrim(nn,2)
  readf, 1, aa
  spaces = [ strsplit( aa, ' ' ), strlen(aa)+1 ]
  lon[j] = float( strmid( aa, spaces[0], spaces[1]-spaces[0] ) )
  rad[j] = float( strmid( aa, spaces[1], spaces[2]-spaces[1] ) - 133000.0d0 )
  imnum[j] = fix( strmid( aa, spaces[2], spaces[3]-spaces[2] ) )
endfor

if keyword_set(inner) then begin
  lon_inner = lon
  rad_inner = rad
  imnum_inner = imnum
  save, lon_inner, rad_inner, imnum_inner, filename=savefile
endif else begin
  lon_outer = lon
  rad_outer = rad
  imnum_outer = imnum
  save, lon_outer, rad_outer, imnum_outer, filename=savefile
endelse

end
