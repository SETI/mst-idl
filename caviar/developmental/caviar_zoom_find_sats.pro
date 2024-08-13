; $Id: zoom.pro,v 1.11 2000/08/07 17:29:27 chris Exp $

pro caviar_zoom_find_sats,im,rawim,cam_params,cmat,nl,et,sc,polera,poledec, $
                coords,planet_coords,ring_coords,stars,planet,s_names,located_satellites, $
		plan_names,$
		xsize=xs, ysize=ys, fact = fact, $
		interp = interp, continuous = cont, $
  	        keep=keep, zoom_window=zoom_win, new_window=new_win
;+
; NAME:
;	ZOOM
;
; PURPOSE:
;	Display part of an image (or graphics) from the current window
;	enlarged in another window.
;
;	The cursor is used to mark the center of the zoom.
;
; CATEGORY:
;	Image display.
;
; CALLING SEQUENCE:
;	ZOOM [, FACT = Fact, /INTERP, XSIZE = Xs, YSIZE = Ys, /CONTINUOUS, $
;		/KEEP, ZOOM_WINDOW=Zoom_Win, /NEW_WINDOW ]
;
; INPUTS:
;	All input parameters are passed as keywords.
;
; KEYWORDS:
;	FACT:	Zoom factor.  This parameter must be an integer.  The default
;		zoom factor is 4.
;
;	INTERP:	Set this keyword to use bilinear interpolation, otherwise
;		pixel replication is used.
;
;	XSIZE:	The X size of the zoom window.  The default is 512.
;
;	YSIZE:	The Y size of the zoom window.  The default is 512.
;
;   CONTINUOUS:	Set this keyword to make the zoom window track the mouse
;		without requiring the user to press the left mouse button.
;		This feature only works well on fast computers.
;
;         KEEP: Keep the zoom window after exiting the procedure.
;
;  ZOOM_WINDOW:	When used with KEEP, returns the index of the zoom window.
;		Otherwise, if KEEP is not set, then -1 is returned.
;
;   NEW_WINDOW:	Normally, if ZOOM is called with /KEEP and then called again,
;		it will use the same window to display the zoomed image.
;		Calling ZOOM with /NEW_WINDOW forces it to create a new window
;		for this purpose.
;
; OUTPUTS:
;	No explicit outputs.   A new window is created if necessary. It
;	is destroyed upon exit if KEEP is not specified.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	A window is created/destroyed.
;
;	When ZOOM is reusing a zoom window from a previous call to ZOOM,/KEEP,
;	then the XSIZE and YSIZE parameters are reset to the actual size of the
;	window.
;
; RESTRICTIONS:
;	ZOOM only works with color systems.
;
; PROCEDURE:
;	Straightforward.
;
; MODIFICATION HISTORY:
;	?
;       William Thompson, March 1992, added common block ZOOM_WINDOW
;		and KEEP keyword.
;	William Thompson, 20 May 1993, added ZOOM_WINDOW and NEW_WINDOW
;		keywords.
;
;-
COMPILE_OPT strictarr

on_error,2              ;Return to caller if an error occurs
common zoom_window, zoom_w
;
if n_elements(xs) le 0 then xs = 500;default window size changed to 500x500
if n_elements(ys) le 0 then ys = 500;default window size changed to 500x500
if n_elements(fact) le 0 then fact=4
if keyword_set(cont) then waitflg = 2 else waitflg = 3
ifact = fact
old_w = !d.window
if keyword_set(new_win) then zoom_w = -1	;Don't use old window (if any)
if n_elements(zoom_w) eq 0 then zoom_w = -1		;No zoom window yet
;
;  If an old window is to be used, then make sure it still exists.  (Added by
;  William Thompson, 20 May 1993.)
;
if zoom_w ge 0 then begin
	device, window_state=win_state
	if not win_state[zoom_w] then zoom_w = -1
endif
;
;  Make sure the parameters xs and ys agree with the size of the window, in
;  case a window is being reused from a previous call to ZOOM,/KEEP.  (Added by
;  William Thompson, 20 May 1993.)
;
IF ZOOM_W GE 0 THEN BEGIN
	OLD_WINDOW = !D.WINDOW
	WSET, ZOOM_W
	XS = !D.X_SIZE
	YS = !D.Y_SIZE
	WSET, OLD_WINDOW
ENDIF
	a=bytarr(25,25)
tvcrs,1			;enable cursor
ierase = 0		;erase zoom window flag
IF KEYWORD_SET(cont) THEN BEGIN
	PRINT,'Cursor position is zoom center, '+$
	      'Middle button for new zoom factor, Right button to quit'
ENDIF ELSE BEGIN
	PRINT,'Left for zoom center, Middle for new zoom factor, Right to quit'
ENDELSE
again:
	cursor,x,y,waitflg,/dev	;Wait for change

	;line=nl-1-y
	;sample=x

	y=nl-1-y


	case !MOUSE.button of
4:	goto, done
2:	if !d.name eq 'SUN' or !d.name eq 'X' then begin	;Sun view?
		s  = ['New Zoom Factor:',strtrim(indgen(19)+2,2)]
		ifact = wmenu(s, init=ifact-1,title=0)+1
		IF (!Version.Os NE 'MacOS') THEN $
			tvcrs,x,y-nl+1,/dev $	;Restore cursor
                ELSE tvcrs,1
		ierase = 1
	endif else begin
		Read,'Current factor is',ifact+0,'.  Enter new factor: ',ifact
		if ifact le 0 then begin
			ifact = 4
			print,'Illegal Zoom factor.'
			endif
			ierase = 1	;Clean out previous display
	endelse

1:	if 1 eq 1 then begin	
	;x0 = 0 > (x-xs/(ifact*2)) 	;left edge from center
	;y0 = 0 > (y-ys/(ifact*2)) 	;bottom
	x0 = (x-xs/(ifact*2)) 	;left edge from center
	y0 = (y-ys/(ifact*2)) 	;bottom
	nx = xs/ifact			;Size of new image
	ny = ys/ifact
	;nx = nx < (!d.x_vsize-x0)
	;ny = ny < (!d.y_size-y0)
	;x0 = x0 < (!d.x_vsize - nx)
	;y0 = y0 < (!d.y_vsize - ny)
	;a = tvrd(x0,y0,nx,ny);Read image
	x1=x0+nx-1
	y1=y0+ny-1
	a=0*a
	xmin=x0
	xstart=0L
	if xmin lt 0 then begin
		xstart=-xmin
		xmin=0L
	endif
	ymin=y0
	ystart=0L
	if ymin lt 0 then begin
		ystart=-ymin
		ymin=0L
	endif
	xmax=x1
	xstop=24
	if xmax gt (nl-1) then begin
		xstop=24-(xmax-nl+1)
		xmax=(nl-1)
	endif
	ymax=y1
	ystop=24
	if ymax gt (nl-1) then begin
		ystop=24-(ymax-nl+1)
		ymax=(nl-1)
	endif

	;a=im[x0:x1,y0:y1]
	a[xstart:xstop,ystart:ystop]=im[xmin:xmax,ymin:ymax]	
	if zoom_w lt 0 then begin	;Make new window?
		window,/free,xsize=xs,ysize=ys,title='Zoomed Image'
		zoom_w = !d.window
	endif else begin
		wset,zoom_w
		if ierase then erase		;Erase it?
		ierase = 0
	endelse
	xss = nx * ifact	;Make integer rebin factors
	yss = ny * ifact
	;tv,rebin(a,xss,yss,sample=1-keyword_set(interp)),/order
	tv,rebin(a,xss,yss,/sample),/order
	plots,12.5*ifact,12.5*ifact,psym=1,symsize=2.0,/device;plots a cross at the cursor pixel

	;line=nl-1-y
	;sample=x
	line=y
	sample=x

	man_find_satellites,y,x,rawim,et,nl,planet,planet_coords,y_cent,x_cent,located_satellites,$
		cam_params,cmat,sat_name


	xyouts,20,460,'                              '
  	xyouts,20,445,'                              '                                	
	xyouts,20,460,'line = '+strcompress(string(line))+'  '+$
	      'sample = '+strcompress(string(sample))+'  DN = '$
              +strcompress(string(long(rawim[x,y]))),/device
	;xyouts,20,445,'Object '+strcompress(string(sat_name)),/device
	xyouts,20,445,'Object '+strcompress(string(sat_name)),/device
	xyouts,20,430,'Centroided line = '+strcompress(string(y_cent))+'  '+$
	      'Centroided sample = '+strcompress(string(x_cent),/remove_all),/device
	wset,old_w

		

	goto, continue

	endif



else:	begin
	;x0 = 0 > (x-xs/(ifact*2)) 	;left edge from center
	;y0 = 0 > (y-ys/(ifact*2)) 	;bottom
	x0 = (x-xs/(ifact*2)) 	;left edge from center
	y0 = (y-ys/(ifact*2)) 	;bottom
	nx = xs/ifact			;Size of new image
	ny = ys/ifact
	;nx = nx < (!d.x_vsize-x0)
	;ny = ny < (!d.y_size-y0)
	;x0 = x0 < (!d.x_vsize - nx)
	;y0 = y0 < (!d.y_vsize - ny)
	;a = tvrd(x0,y0,nx,ny);Read image
	x1=x0+nx-1
	y1=y0+ny-1
	a=0*a
	xmin=x0
	xstart=0L
	if xmin lt 0 then begin
		xstart=-xmin
		xmin=0L
	endif
	ymin=y0
	ystart=0L
	if ymin lt 0 then begin
		ystart=-ymin
		ymin=0L
	endif
	xmax=x1
	xstop=24
	if xmax gt (nl-1) then begin
		xstop=24-(xmax-nl+1)
		xmax=(nl-1)
	endif
	ymax=y1
	ystop=24
	if ymax gt (nl-1) then begin
		ystop=24-(ymax-nl+1)
		ymax=(nl-1)
	endif

	;a=im[x0:x1,y0:y1]
	a[xstart:xstop,ystart:ystop]=im[xmin:xmax,ymin:ymax]
	if zoom_w lt 0 then begin	;Make new window?
		window,/free,xsize=xs,ysize=ys,title='Zoomed Image'
		zoom_w = !d.window
	endif else begin
		wset,zoom_w
		if ierase then erase		;Erase it?
		ierase = 0
	endelse
	xss = nx * ifact	;Make integer rebin factors
	yss = ny * ifact
	tv,rebin(a,xss,yss,sample=1-keyword_set(interp)),/order
	plots,12.5*ifact,12.5*ifact,psym=1,symsize=2.0,/device;plots a cross at the cursor pixel

	;line=nl-1-y
	;sample=x
	line=y
	sample=x

	;p2radec,cam_params,cmat,nl,line,sample,RA,dec
	xyouts,20,460,'                              '
  	xyouts,20,445,'                              '                                	
	xyouts,20,460,'line = '+strcompress(string(line))+'  '+$
	      'sample = '+strcompress(string(sample))+'  DN = '$
              +strcompress(string(long(rawim[x,y]))),/device
	;xyouts,20,445,'RA = '+strcompress(string(RA))+'  '+$
	      ;'dec = '+strcompress(string(dec),/remove_all),/device

	zcoords=coords
	zplanet_coords=planet_coords
	zring_coords=ring_coords

	zcoords[*,0]=(coords[*,0]-y+12.5)*20.0d0
	zcoords[*,1]=(coords[*,1]-x+12.5)*20.0d0
	zplanet_coords[*,0]=(planet_coords[*,0]-y+12.5)*20.0d0
	zplanet_coords[*,1]=(planet_coords[*,1]-x+12.5)*20.0d0
	zring_coords[*,0]=(ring_coords[*,0]-y+12.5)*20.0d0
	zring_coords[*,1]=(ring_coords[*,1]-x+12.5)*20.0d0

	plots,round(zcoords[*,1]),round(499.0-zcoords[*,0]),psym=1,symsize=1.0,color=make_array(n_elements(stars[*,0]),value=cyan()),/device
	plots,round(zplanet_coords[*,1]),round(499.0-zplanet_coords[*,0]),psym=1,symsize=1.0,color=make_array(n_elements(planet[*,0]),value=green()),/device
	plots,round(zring_coords[*,1]),round(499.0-zring_coords[*,0]),psym=1,symsize=1.0,color=make_array(n_elements(ring_coords[*,0]),value=blue()),/device

	mag=(stars[*,3]/100.0)
	mag=strcompress(string(mag),/remove_all)
	xyouts,round(zcoords[*,1])+6,round(499.0-zcoords[*,0])-3,s_names,color=make_array(n_elements(stars[*,0]),value=cyan()),/device
	xyouts,round(zcoords[*,1])+6,round(499.0-zcoords[*,0])-13,mag,color=make_array(n_elements(stars[*,0]),value=cyan()),/device
	xyouts,round(zcoords[*,1])+6,round(499.0-zcoords[*,0])-23,'line    = '+strcompress(string(coords[*,0]),/remove_all),color=make_array(n_elements(stars[*,0]),value=cyan()),/device
	xyouts,round(zcoords[*,1])+6,round(499.0-zcoords[*,0])-33,'sample = '+strcompress(string(coords[*,1]),/remove_all),color=make_array(n_elements(stars[*,0]),value=cyan()),/device
	xyouts,round(zplanet_coords[*,1])+6,round(499.0-zplanet_coords[*,0])-3,plan_names[*],color=make_array(n_elements(planet[*,0]),value=green()),/device


	wset,old_w



continue:
	
    endcase
endcase

goto,again

done:
IF NOT KEYWORD_SET(KEEP) THEN BEGIN
        if zoom_w ge 0 then wdelete,zoom_w              ;Done with window
        ZOOM_W = -1
ENDIF
zoom_win = zoom_w	;Return index of zoom window to user
end

