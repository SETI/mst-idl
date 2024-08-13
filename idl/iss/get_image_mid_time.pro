dir = [ 'SOISPTURN', 'ARINGLIT' ]
imt_filenames = ''
image_mid_time = [ 0., 0., 0., 0., 0. ]
nimt = 0

for k=0,1 do begin

  spawn, 'grep IMAGE_MID_TIME ~/Data/images/SOI/'+dir[k]+'/*LBL > image_mid_time'

  openr, 1, 'image_mid_time'
  a = ''
  while not eof(1) do begin
    readf, 1, a
    b = strpos( a, '14673' ) - 1
    imt_filenames = [ imt_filenames, strmid(a,b,11) ]
    image_mid_time = [ [image_mid_time], [ strmid(a,b+36,4), strmid(a,b+41,3),$
	strmid(a,b+45,2), strmid(a,b+48,2), strmid(a,b+51,6) ] ]
    nimt = nimt + 1
  endwhile
  close, 1
  spawn, 'rm image_mid_time'

endfor
imt_filenames = clip(imt_filenames)
image_mid_time = image_mid_time[*,1:nimt]

leap = image_mid_time[0,0] mod 4
if leap eq 0 then leap=1 else leap=0
foo = doy( image_mid_time[1,*], leap=leap, month=month, day=day )
imt = julday( month, day, image_mid_time[0,*], image_mid_time[2,*], $
	image_mid_time[3,*], image_mid_time[4,*] )

end
