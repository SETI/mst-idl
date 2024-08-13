pro plotanno,xp,yp,dx,dy,anno,lin,col,norm=norm,psym=psym,size=size,thick=thick,underline=underline,symsize=symsize,textcolor=textcolor
;; jschmidt 18.05.05
;; draw an array of lines and annotations to a plot
;; $Id: plotanno.pro,v 1.6 2007/09/03 17:00:35 jschmidt Exp $

if n_params() eq 0 then begin
print,'pro plotanno'
print,''
print,'call:'
print,'plotanno,xp,yp,dx,dy,anno,lin,col,norm=norm,psym=psym,size=size,thick=thick,underline=underline,symsize=symsize'
print,''
print,''
print,''


return
endif

        nn=n_elements(anno)

        for in=0,nn-1 do begin

                if keyword_set(psym) then begin
		
			ps=psym(in)

		endif else begin

			ps=0

		endelse

		if keyword_set(symsize) then begin

			syms=symsize(in)

		endif else begin

			syms=0

		endelse

                if (col(in) gt 0) then begin

			if keyword_set(underline) then begin

				if underline(in) ne 0 then begin

					if ps eq 10 then begin
			 
						xyouts,xp(0),yp(0),/norm, $
							'HISTOGRAM:'

					endif else begin

        		                	 plots,xp,yp,norm=norm,ps=ps, $
					 		thick=thick,syms=syms
		
					endelse
				endif
			endif

                        if ps eq 10 then begin
			 
				xyouts,xp(0),yp(0),/norm,'HISTOGRAM:'

			endif else begin

				 plots,xp,yp,norm=norm,lin=lin(in),ps=ps, $
				 col=col(in), $
			 	thick=thick,syms=syms
			endelse

                endif else begin

                        if ps eq 10 then begin
			 
				xyouts,xp(0),yp(0),/norm,'HISTOGRAM:'

			endif else begin

				plots,xp,yp,norm=norm,ps=ps,lin=lin(in), $
					thick=thick,syms=syms

			endelse

                endelse

				if keyword_set(textcolor) then begin
                xyouts,xp(1)+dx,yp(0),anno(in),norm=norm,size=size,col=textcolor
				endif else begin
                xyouts,xp(1)+dx,yp(0),anno(in),norm=norm,size=size
				endelse

                yp=yp-dy

        endfor

end

