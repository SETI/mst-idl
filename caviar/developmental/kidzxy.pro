PRO kidzxy,im,crays
	wset,1
	nl=n_elements(im(0,*))

again:

        cursor,x1,y1,2,/device
        yy1=nl-1-y1
                                                                                                         
        case !MOUSE.button of

                                                                                                         
4:      goto, done
                                                                                                         
1:      begin
        line1=yy1
        sample1=x1
	endcase

else:   begin
        goto,again
        endcase

endcase

done:

onceagain:
                                                                                                        
        cursor,x2,y2,2,/device
        yy2=nl-1-y2
                                                                                                         
        case !MOUSE.button of
                                                                                                         
4:      goto, done_2
                                                                                                         
1:      begin
                                                                                                         
	tv, im, /order
                                                                                                         
        line2=yy2
        sample2=x2

        plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1], color=green(), /device

	endcase


16:     begin
        goto, again
        endcase
else:   begin
        goto, onceagain
        endcase
endcase
        goto, onceagain
done_2:


crays=fltarr(nl^2,2)
crays(*,0)=(findgen(nl^2) mod nl)
crays(*,1)=crays(sort(crays(*,0)),0)
q=where(crays(*,0) gt max([yy1,yy2]) or crays(*,0) lt min([yy1,yy2]) $
        or crays(*,1) gt max([x1,x2]) or crays(*,1) lt min([x1,x2]))
crays=crays(q,*)



end
