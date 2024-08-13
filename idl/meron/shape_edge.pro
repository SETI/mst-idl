Function Shape_edge, shape, edge, close = clo

;+
; NAME:
;	SHAPE_EDGE
; VERSION:
;	3.0
; PURPOSE:
;	Modifies a SHAPE (see SHAPE_VER for a definition) by cutting it along 
;	a straight edge.
; CATEGORY:
;	General Graphics.
; CALLING SEQUENCE:
;	Result = SHAPE_EDGE( SHAPE, EDGE [, CLOSE = CLO])
; INPUTS:
;    SHAPE
;	Two dimensional shape i.e. a (2,*) numeric array.
;    EDGE
;	An (2,2) numeric array representing a straight line in the 
;	[[point],[direction]] format, i.e. EDGE(*,0) is a point on the line,
;	EDGE(*,1) is a vector in the direction of the line.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    /CLOSE
;	Switch.  If set, forces closure of the output shape.  If the input
;	shape happens to be closed, the output one is being closed 
;	automatically unless CLOSE is explicitly set to zero.
; OUTPUTS:
;	Returns a new shape, made of the points of the original shape which 
;	were not cut off by the edge, plus the points introduced by the edge
;	line cutting through line segments of the original shape.
;	Important:  The edge line is DIRECTED.  Its direction is the direction
;	of the direction vector.  The points to the left of the line (relative
;	to the line direction) are maintained, those to the right are cut off.
;	This is consistent with viewing the edge line as part of a mathematical
;	contour (in a mathematically positive direction) and keeping the points
;	inside the contour.
;	If there are no points left in the result shape, a single point (i.e.
;	2D vector) with X and Y coordinates equal to the square root of the 
;	maximal floating value (machine dependent) is returned.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Uses LINCROSS from MIDL to find crossing points of the EDGE line and 
;	the shape and adjusts the shape appropriately.  Also calls ARREQ, 
;	DEFAULT, SHAPE_CLOSE, SHAPE_VER and VINP from MIDL.
; MODIFICATION HISTORY:
;	Created 10-NOV-1997 by Mati Meron.
;-

    on_error, 1
    sinf = machar()
    res = sqrt(reform(replicate(sinf.xmax,2),2,1))

    ep = edge(*,0)
    ed = edge(*,1)
    en = [-ed(1),ed(0)]
    efac = Vinp(ep,en)

    ndim = Shape_ver(shape,len = np)
    if ndim ne 2 then begin
	if ndim eq 3 then message, 'Only 2-D shapes accepted!', /continue $
	else message, 'Improper or missing shape!', /continue
	return, 0
    endif

    pos = fltarr(np)
    for i = 0l, np - 1 do pos(i) = Vinp(shape(*,i),en)
    pos = (temporary(pos) - Vinp(ep,en)) ge 0
    a = where(pos gt [0,pos], nsec)
    if nsec gt 0 then begin
	b = where(pos gt [pos(1:*),0])
	for i = 0l, nsec - 1 do begin
	    if a(i) ne 0 then begin
		id = Lincross(edge,shape(*,a(i)-1:a(i)),lin = 1,cross = cro)
		if id then res=[[res],[cro]]
	    endif
	    res = [[res],[shape(*,a(i):b(i))]]
	    if b(i) ne np - 1 then begin
		id = Lincross(edge,shape(*,b(i):b(i)+1),lin = 1,cross = cro)
		if id then res=[[res],[cro]]
	    endif
	endfor
	res = res(*,1:*)
	if (Arreq(shape(*,0),shape(*,np-1)) and Default(clo,1) ne 0) $
	or keyword_set(clo) then res = Shape_close(res)
    endif
	
    return, res
end
