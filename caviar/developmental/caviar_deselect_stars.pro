pro caviar_deselect_stars,located_stars,nl,im,symbol,symbol_size

	n_stars=n_elements(located_stars[*,0])

        print, strtrim(n_stars,2)+' stars identified.'
        print, 'Left-click a star to deselect it.  Right-click to exit.'

again:
	cursor,x,y,3,/dev

	case !MOUSE.button of

4:	goto, done


1:	begin

	distance=sqrt(((((nl-1)-located_stars[*,4])-y)^2.0)+((located_stars[*,5]-x)^2.0))

	min_dist=min(distance,min_subscript)

	if min_dist le 2.0 then begin

		reduced_located_stars=dblarr(n_stars-1,8)

		j=0

		for i=0,n_stars-1 do begin

			if i ne min_subscript then begin
				reduced_located_stars[j,*]=located_stars[i,*]
				j=j+1
			endif
		endfor

		located_stars=reduced_located_stars
		n_stars=n_elements(located_stars[*,0])

		fittedx=round(located_stars[*,5])
		fittedy=round(located_stars[*,4])
		nx=n_elements(located_stars[*,5])
		tv,im,/order
		plots,fittedx,(nl-1)-fittedy,psym=symbol,symsize=symbol_size,color=make_array(nx,value=cyan()),/device

	endif

goto, again

done:

endcase
endcase

return
end
