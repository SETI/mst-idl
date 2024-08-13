;; ------------------- bootstrap_patSpeed.pro ------------------- ;;
;; LAST MODIFIED:  P.TORREY 5/15/08
;; DEPENDENCIES:   _bsa files created from bootstrap
;; OUTPUT:         the emperically derived pattern speed for a bootstrap sequence
;;
;; SUMMARY:  	   CAUTION: this program is currently setup to be used for the rev007 AZSCN.
;;		   The true time offset between the first image in the sequence and any other
;;		   image can be determned in a straightforward manner from the information saved
;;		   with every image.  We also know the longitudinal/dt offset that we have used in the
;;		   bootstrapping code from the _bsa files.  The emperical pattern speed for any image
;;		   is determined by the ratio of the emperical time offset and the true time offset.
;;		   Stated differently:  The true pattern speed multiplied by the true time offset
;;		   should yield the true longitudinal offset. We use the known time offset and the
;;		   known longitudinal offset to determine the unknown pattern speed.


restore,'stretch.sav'
restore,'et.sav'
lastdt = 0
relativedt = dblarr( (size(filenames))(1) )
empericaldt = dblarr( ( size(filenames))(1) )
patternspeed = dblarr( ( size(filenames))(1) )
trueTimeOffset = dblarr( (size(filenames))(1) )
image_number = findgen( (size(filenames))(1) )

for i=0,(size(filenames))(1)-1 do begin
	if (i le 100) then trueTimeOffset(i) = _et[i] - _et[0]
	if (i ge 101 && i le 150) then trueTimeOffset[i] = _et[i] - _et[101]
	if (i ge 151 && i le 200) then trueTimeOffset[i] = _et[i] - _et[151]
	if (i ge 201) then trueTimeOffset[i] = _et[i] - _et[201]
endfor

empericaldt(0) = 0

for i=1,100 do begin;(size(filenames))(1)-1 do begin
	if ( keyword_set(findfile(filenames(i)+'_bsa')) ) then begin
		restore,filenames(i)+'_bsa'
		empericaldt(i) = dt
	endif else begin
		empericaldt(i) = 0
	endelse
	if(trueTimeOffset(i) ne 0) then begin
		patternspeed[i] = empericaldt(i)/trueTimeOffset(i)
	endif else begin
		patternspeed[i] = 0
	endelse
endfor

patternspeed = patternspeed(1: (size(patternspeed))(1)-1 )    ;Cut out the first pattern speed
image_number = image_number(1: (size(image_number))(1)-1 )    ;Cut out the corresponding image number
i = 0
while (i lt (size(patternspeed))(1)-1) do begin

	if (patternspeed(i) eq 0) then begin
		patternspeed = [ patternspeed(0:i-1), patternspeed(i+1: (size(patternspeed))(1)-1 ) ]
		image_number = [ image_number(0:i-1), image_number(i+1: (size(image_number))(1)-1 ) ]
		i = i-1
	endif
	i = i+1
endwhile

patternspeed = patternspeed(0:(size(patternspeed))(1)-2)    	;Cut out the last pattern speed
image_number = image_number(0:(size(image_number))(1)-2)	;Cut out the last image number

for i=0,(size(patternspeed))(1)-1 do begin
	print,image_number(i),patternspeed(i)
endfor

print,''
print,''
print,''

print,mean(patternspeed)
print,median(patternspeed)
print,stddev(patternspeed)


end
