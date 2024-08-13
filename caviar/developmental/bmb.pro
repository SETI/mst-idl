;This program was written in Spring 2011 by Breanna Byington. It sped
;up the saving and graphing of repointing differences between star
;pointing and edge pointing in the Cassini division.

;restore, 'repoint_array.sav'
;repoint_rad=make_array(63,2)
;repoint_rad=double(repoint_rad)
;repoint_lon=make_array(63,2)
;repoint_lon=double(repoint_lon)

;j=0

;a:
;if (j lt 63) then begin
;repoint_rad[j,0]=mean(repoint_array[j,1,*])
;repoint_rad[j,1]=stddev(repoint_array[j,1,*])
;repoint_lon[j,0]=mean(repoint_array[j,3,*])
;repoint_lon[j,1]=stddev(repoint_array[j,3,*])
;j=j+1
;print, j
;goto, a
;endif

;end

repoint_array=make_array(63,4)
restore, 'stretch.sav
device, retain=2

j=0

a:
if (j lt 63) then begin
image_name=filenames[j]
file=image_name + '.rep'
restore, filename=file
repoint_array[j,0]=mean(repoint_results[1,*])
repoint_array[j,1]=stddev(repoint_results[1,*])
repoint_array[j,2]=mean(repoint_results[3,*])
repoint_array[j,3]=stddev(repoint_results[3,*])
j=j+1
print,j
goto, a
endif

end
