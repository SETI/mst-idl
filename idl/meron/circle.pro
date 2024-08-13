Pro Circle, center = cent, radius = prad, x_radius = xrad, y_radius = yrad, $
    fill = fil, device = dev, normal = nor, _extra = _e

;+
; NAME:
;	CIRCLE
; VERSION:
;	3.0
; PURPOSE:
; 	Draws a circle, around CENTER, with radius given by RADIUS, X_RADIUS, 
;	or Y_RADIUS.  The drawing is done in the currently defined plot area. 
;	One and ONLY ONE of the three radius values MUST be provided.  RADIUS 
;	and X_RADIUS are equivalent.  DATA coordinates are used unless one of 
;	the keywords /DEVICE or /NORMAL is set.  The circle is drawn so as to
;	appear visually as a circle, regardless of the coordinates used.
; CATEGORY:
;	General Graphics.
; CALLING SEQUENCE:
;	CIRCLE, CENTER = C, {RADIUS=R, X_RADIUS=XR, Y_RADIUS = YR} [, keywords]
; INPUTS:
;	None.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    CENTER
;	Two dimensional vector, circle center location, mandatory.
;    RADIUS							|
;	Scalar, value of radius (measured in the X direction).	| One
;    X_RADIUS							| and only one
;	Scalar, value of radius (measured in the X direction).	| must be
;    Y_RADIUS							| provided.
;	Scalar, value of radius (measured in the Y direction).	|
;    /FILL
;	Switch.  causes the circle to be filled with a solid pattern.
;    /DEVICE
;	Standard IDL plotting interpretation.
;    /NORMAL
;	Ditto.
;    _EXTRA
;       A formal keyword used to pass all plotting keywords.  Not to be used
;       directly.  See comment in RESTRICTIONS.
; OUTPUTS:
;	None.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
;	None.
; RESTRICTIONS:
;	While nominally all graphics keywords can be passed (through _EXTRA),
;	care should be exercised to use only the applicable ones (ELLIPSE and 
;	PLOTS keywords usually, POLYFILL keywords when /FILL is used.
; PROCEDURE:
;	Uses calls to COO_CONV and ONE_OF from MIDL.  Converts 	all parameters 
;	to device coordinates and calls ELLIPSE (also from MIDL) to do the 
;	actual plotting.
; MODIFICATION HISTORY:
;	Created 15-JUL-1991 by Mati Meron.
;       Modified 15-DEC-1993 by Mati Meron.  Now CIRCLE takes advantage of the
;       keyword inheritance property and accepts all IDL plotting keywords.
;-

    on_error, 1
    rnum = One_of(xrad,yrad,prad, value = rad) mod 2
    if rnum eq -1 then message, 'Missing radius!'

    posib = ['DATA', 'DEVICE', 'NORMAL']
    sor = posib(1 + One_of(dev,nor))

    tem = [cent(rnum),cent(rnum) + rad]
    tem = Coo_conv(tem, axis = rnum, from = sor, to = 'DEVICE')
    rad = tem(1) - tem(0)

    x = Coo_conv(cent(0), axis = 'X', from = sor, to = 'DEVICE')
    y = Coo_conv(cent(1), axis = 'Y', from = sor, to = 'DEVICE')

    Ellipse, center = [x,y], radii = [rad,rad], /device, fill = fil, _extra = _e

    return
end
