function medianfilter, redgearr, filtersize, rind

filterUP = median(redgearr(rind+1,*))+filtersize*stddev(redgearr(rind+1,*))
filterDOWN = median(redgearr(rind+1,*))-filtersize*stddev(redgearr(rind+1,*))

i=0l

while(i le (size(redgearr(rind,*)))(2)-1) do begin
	if (redgearr(rind+1,i) ge filterUP) || (redgearr(rind+1,i) le filterDOWN) then begin
		if (i ne 0 && i ne (size(redgearr(rind,*)))(2)-1) then begin
			redgearr = [[redgearr(*,0:i-1)],[redgearr(*,i+1:(size(redgearr(rind,*)))(2)-1)]]
		endif else begin
			if i eq 0 then redgearr = redgearr(*,1:(size(redgearr(rind,*)))(2)-1)
			if i eq ((size(redgearr(rind,*)))(2)-1) then redgearr = redgearr(*,0:(size(redgearr(rind,*)))(2)-2)
		endelse
		i = i-1
	endif
	i=i+1
endwhile

return,redgearr

end
