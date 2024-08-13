Pro Rectan, xlims = xls, ylims = yls, rotate = rot, degrees = deg, $
    radius = rad, relative = rel, fill = fil, device = dev, normal = nor, $
    rotation_center = rocent, _extra = _e
;+
; NAME:
;	RECTAN
; VERSION:
;	3.0
; PURPOSE:
;	Draws a rectangle between the limits specified by XLIMS and YLIMS.  The 
;	drawing is done in the currently defined plot area.  DATA coordinate 
;	system is assumed unless specified otherwise by one of the keywords 
;	/DEVICE or /NORMAL.
; CATEGORY:
;	General Graphics.
; CALLING SEQUENCE:
;	RECTAN, XLIMS = XLS, YLIMS = YLS [, optional keywords]
; INPUTS:
;	None.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    XLIMS
;	2 dimensional vector, format [xmin,xmax], mandatory.
;    YLIMS
;	2 dimensional vector, format [xmin,xmax], mandatory.
;    ROTATE
;	Optional.  Angle of rotation in the mathematical positive direction.
;	Assumed in radians, unless DEGREES is set.  Rotation center is the 
;	center of the rectangle unless specified otherwise by the keyword
;	rotation_center (see below).
;    /DEGREES
;	Switch.  Specifies that the rotation angle is given in degrees.
;    ROTATION_CENTER
;	Optional.  Accepts a two element vector specifying the center of 
;	rotation.  Ignored if ROTATE is not given.  Defaults to center of shape.
;    RADIUS
;	Value of radius for rounded corners.
;    /RELATIVE
;	Switch.  Specifies that the radius value is relative to the shorter 
;	side of the rectangle.
;    /FILL
;	Switch.  Causes the rectangle to be filled with a solid pattern.
;    /DEVICE
;	Standard IDL plotting interpretation.
;    /NORMAL
;	Ditto.
;    _EXTRA
;	A formal keyword used to pass all plotting keywords.  Not to be used
;	directly.
; OUTPUTS:
;	None.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	RECTAN calls either PLOTS or (when /FILL is used) POLYFILL.  Since
;	some graphics keywords work only with PLOTS, or only with POLYFILL,
;	some care must be exercised.
; PROCEDURE:
;	Uses calls to DEFAULT, ONE_OF, SHAPE_COCON, SHAPE_CLOSE, and 
;	SHAPE_TRANS from MIDL.
; MODIFICATION HISTORY:
;	Created 15-JUL-1991 by Mati Meron.
;	Modified 15-OCT-1991 by Mati Meron.  Added keyword COLOR.
;	Modified 15-OCT-1992 by Mati Meron.  Added rotation capability.
;	Modified 30-OCT-1992 by Mati Meron.  Added corner rounding capability.
;	Modified 15-DEC-1993 by Mati Meron.  Now RECTAN takes advantage of the
;	keyword inheritance property and accepts all IDL graphics keywords.
;-

    on_error, 1
    posib = ['DATA', 'DEVICE', 'NORMAL']
    sor = posib(1 + One_of(dev,nor))

    rls = transpose([[min(xls, max = sec), sec],[min(yls, max = sec), sec]])
    corn = rls([[2,3],[0,3],[0,1],[2,1]])
    rad = Default(rad,0.,/dtype)

    if rad gt 0 then begin
	radx = 0.5*min(abs(rls(*,1) - rls(*,0)))
	if keyword_set(rel) then rad = radx*rad
	rad = rad < radx
	cent = corn - rad*[[1,1],[-1,1],[-1,-1],[1,-1]]

	rect = [0,0]
	for i = 0, 3 do begin
	    rsor = [[corn(*,i)],[cent(*,i)]]
	    rdev = Shape_cocon(rsor, from = sor, to = 'dev')
	    npoints = 1 + fix(!pi/4*sqrt(max(abs(rdev(*,1) - rdev(*,0)))))
	    tem = !pi/(2*npoints)*(indgen(npoints + 1) + i*npoints)
	    arc = transpose([[cent(0,i)+rad*cos(tem)],[cent(1,i)+rad*sin(tem)]])
	    rect = [[rect],[arc]]
	endfor
	rect = Shape_close(rect(*,1:*))
    endif else rect = Shape_close(corn)
    if n_elements(rot) ne 0 then begin
	rocent = Default(rocent,0.25*total(corn,2),low=4)
	rect = Shape_trans(rect,0,1,-rocent)
	rect = Shape_trans(rect,rot,1,rocent,degrees= deg)
    endif

    if keyword_set(fil) then $
    polyfill, rect, device=dev, normal=nor, _extra = _e $
    else plots, rect, device = dev, normal = nor, _extra = _e

    return
end
