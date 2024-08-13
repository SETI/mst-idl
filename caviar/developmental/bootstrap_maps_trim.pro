;; ------------- bootstrap_maps_trim.pro ---------------;;
;; LAST MODIFIED:  P.TORREY 5/15/08
;; DEPENDENCIES:  redge,minlong,maxlong
;; OUTPUT:	  redge
;;
;; SUMMARY:	This routine takes an edge profile (redge), and minimum longitude
;;		(minlong), and a maximum longitude (maxlong), for arguments and returns
;;		the edge profile so that it only contains points within the specified
;;		longitude range.
 

function bootstrap_maps_trim, redge, minlong, maxlong

i=0l

redge = redge[*,sort(redge(0,*))]

for i=0l,(size(redge))(2)-1 do begin
	if (redge(0,i) gt 180.0) then begin
		redge(0,i) = redge(0,i)-360.0
	endif
	if (redge(0,i) lt -180.0) then begin
		redge(0,i) = redge(0,i)+360.0
	endif
endfor

redge = redge[*,sort(redge(0,*))]

if( max(redge(0,*)) lt minlong || min(redge(0,*)) gt maxlong) then redge = 0

mini = 0
maxi = (size(redge(0,*)))(2)-1
stop = 1

i=0l

if keyword_set(redge) then begin
	while i lt (size(redge))(2) do begin
		if (redge(0,i) lt minlong)  then begin
			mini = i;
		endif

		if (redge(0,i) gt maxlong && stop) then begin
			maxi = i
			stop=0
		endif
		i = i+1
	endwhile
redge = redge(*,mini:maxi)
endif


i=1l
if keyword_set(redge) then begin
	while i lt (size(redge))(2)-2 do begin
		mean = 	mean(    [redge(1,i-1),redge(1,i),redge(1,i+1)] )
		median = median( [redge(1,i-1),redge(1,i),redge(1,i+1)] )
		if (abs(mean-median) gt 1.3) then begin
			redge = [[redge(*,0:i)],[redge(*,i+2:(size(redge))(2)-1)]]
			i=i-1
			print,i,abs(mean-median),(size(redge))(2)
		endif
		i = i+1
	endwhile
endif

return,redge

end
