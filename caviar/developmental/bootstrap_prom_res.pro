;; ----------------- bootstrap_prom_res.pro ---------------------;;
;; LAST MODIFIED:  P.TORREY 5/15/08
;; DEPENDENCIES:   ---
;; OUTPUT:         lobe_location.sav
;;
;; SUMMARY:  This code determines the predicted locations of Lobes associated with the 
;;  Promethius resonance on the Keeler Inner Edge
;;  It saves the locations of these predicted lobes after adjusting for the 
;;  bootstrap offsets

restore,'stretch.sav'
restore, 'et.sav'

dt = _et - _et[0]		;; Array containing the time from first image
promPatternSpeed = 0.0067812	;; Promethius Pattern Speed in degrees/sec

DaphnisMeanMotion = 0.00699767
restore, 'moonimage.sav'    	;; restores moonimage = image_number_containing_daphnis
sat = 635    			;; Daphnis
et = _et[moonimage]		;; sets the time for image containing daphnis
@get_sat_prepare
@get_sat_coords
longAdjust = sat_polar(1) - (et-_et[0])*DaphnisMeanMotion
;longAdjust = 0

counter = 0
images = intarr( (size(filenames))(1) )
lobelong = dblarr( (size(filenames))(1) )
bootstrap_lobe_long = dblarr( (size(filenames))(1) )

for i=0,(size(filenames))(1)-1 do begin
	images(i) = 0
	lobelong(i) = 0.0
	bootstrap_lobe_long(i) = 0.0
endfor


silent=1
noplot=1

sat = 616;  Promethius

for image_numb=0,(size(filenames))(1)-2 do begin
	image_name = filenames( image_numb )
	longmin = _keywords[image_numb].ringplane_least_orbital_longitude
	longmax = _keywords[image_numb].ringplane_greatest_orbital_longitude	

	et = _et[image_numb]
	@get_sat_prepare
	@get_sat_coords
	Promethius = sat_polar(1)

	longToMin = Promethius - longmin	;  This will be bigger
	longToMax = Promethius - longmax	;  This will be smaller

	while (longToMin lt 0) do begin
		longToMin = longToMin + 360
		longToMax = longToMax + 360
	endwhile

	n = 0
	while (longToMax gt 0) do begin
		n = n+1
		longToMin = longToMin - 11.25
		longToMax = longToMax - 11.25
	endwhile

	if (longToMin gt 0) then begin
		print,'LOBE PRESENT...'
		offset = dt(image_numb)*promPatternSpeed 
		images(counter) = image_numb
		lobeLong(counter) = sat_polar(1) - n*11.25
		if (lobeLong(counter) lt 0) then lobeLong(counter) = lobeLong(counter) + 360.0 
		bootstrap_lobe_long(counter) = lobeLong(counter) - offset - longAdjust;  -13.816 FOR 008 movie : - 78.57 FOR 007 movie	
		while(bootstrap_lobe_long(counter) lt -180) do bootstrap_lobe_long(counter) = bootstrap_lobe_long(counter)+360
		while(bootstrap_lobe_long(counter) gt 180 ) do bootstrap_lobe_long(counter) = bootstrap_lobe_long(counter)-360
		print,image_numb,lobeLong(counter),bootstrap_lobe_long(counter)
		counter = counter+1
	endif else begin
		print,longmin
		print,longmax
		print,fix(image_numb)
	endelse
endfor

;i = 0;
;dt = 0;
;if keyword_set( AZSCAN ) then begin
;	while( images(i) ne 0) do begin
;		if keyword_set(findfile( filenames(images(i))+'_bsa')) then begin
;			restore,filenames(images(i))+'_bsa'
;			bootstrap_lobe_long(i) = lobeLong(i) + dt	
;		endif
;		print,images(i),lobeLong(i),bootstrap_lobe_long(i)
;		i = i+1
;	endwhile
;endif


;if keyword_set( movie ) then begin
;
;	i = 1;
;	while( images(i) ne 0) do begin
;		offset = dt(images(i))*patternSpeed 
;		bootstrap_lobe_long(i) = lobeLong(i) - offset - longAdjust;  -13.816 FOR 008 movie : - 78.57 FOR 007 movie	
;		while(bootstrap_lobe_long(i) lt -180) do bootstrap_lobe_long(i) = bootstrap_lobe_long(i)+360
;		while(bootstrap_lobe_long(i) gt 180 ) do bootstrap_lobe_long(i) = bootstrap_lobe_long(i)-360
;		print,i,images(i),lobeLong(i),bootstrap_lobe_long(i)
;		i = i+1
;	endwhile
;endif

i=1;
while (images(i) ne 0) do i=i+1

if keyword_set(i) then begin
	images = images(1:i-1)
	bootstrap_lobe_long = bootstrap_lobe_long(1:i-1)
endif

save,images,bootstrap_lobe_long,filename='lobeLocations.sav'

end
