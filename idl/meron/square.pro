Pro Square, base = bas, height = hei, side = sid, $
    ll_corner = llc, lr_corner = lrc, ur_corner = urc, ul_corner = ulc, $
    fill = fil, device = dev, normal = nor, _extra = _e

;+
; NAME:
;	SQUARE
; VERSION:
;	3.0
; PURPOSE:
; 	Draws a SQUARE, based on a length of a side and a given location of a 
;	corner.  The square is drawn so as to appear visually as a square, even
;	if the lengths of the sides in DATA coordinates differ.  The drawing is
;	done in the currently defined plot area.  DATA coordinates are used 
;	unless one of the keywords /DEVICE or /NORMAL is set.
; CATEGORY:
;	General Graphics.
; CALLING SEQUENCE:
;	Square, {BASE = BAS, HEIGHT = HEI, SIDE = SID}, $
;	{LL_CORNER = LLC, LR_CORNER = LRC, UR_CORNER = URC, UL_CORNER = ULC}, $
;	[optional keywords]
; INPUTS:
;	None.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    BASE						|
;	Length of side, measured in the X-direction.	| One 
;    SIDE						| and only one
;	Same as BASE.					| must be
;    HEIGHT						| provided
;	Length of side, measured in the Y-direction.	|
;
;    LL_CORNER						   |
;	2dim vector, coordinates of the lower left corner. |
;    LR_CORNER						   | One
;	2dim vector, coordinates of the lower right corner.| and only one
;    UR_CORNER						   | must be
;	2dim vector, coordinates of the upper right corner.| provided
;    UL_CORNER						   |
;	2dim vector, coordinates of the upper left corner. |
;
;    /FILL
;	Switch.  Causes the square to be filled with a solid pattern.
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
;       SQUARE passes the extra keywords through _EXTRA, without checking.  
;	Therefore, some care must be exercised.
; PROCEDURE:
;	Uses calls to COO_CONV and ONE_OF from MIDL.  Converts all 
;	parameters to device coordinates and calls RECTAN (also from MIDL) to 
;	do the actual plotting.
; MODIFICATION HISTORY:
;	Created 15-JUL-1991 by Mati Meron.
;	Modified 15-DEC-1993 by Mati Meron.  Now SQUARE takes advantage of the
;	keyword inheritance property and accepts all IDL plotting keywords.
;-

    on_error, 1
    dnum = One_of(bas,hei,sid,  value = psid) mod 2
    if dnum eq -1 then message, 'Either base or height must be provided!'
    cnum = One_of(llc,lrc,urc, ulc, value = corner)
    if cnum eq -1 then message, 'One corner must be provided!'

    posib = ['DATA', 'DEVICE', 'NORMAL']
    sor = posib(1 + One_of(dev,nor))

    tem = [corner(dnum),corner(dnum) + psid]
    if (cnum - dnum) eq 1 or (cnum - dnum) eq 2 then tem = tem - psid
    tem = Coo_conv(tem, axis = dnum, from = sor, to = 'DEVICE')
    psid = tem(1) - tem(0)

    x = Coo_conv(corner(0), axis = 'X', from = sor, to = 'DEVICE')
    y = Coo_conv(corner(1), axis = 'Y', from = sor, to = 'DEVICE')
    xlims = [x, x + psid]
    ylims = [y, y + psid]
    if cnum eq 1 or cnum eq 2 then xlims = xlims - psid
    if cnum eq 2 or cnum eq 3 then ylims = ylims - psid

    Rectan, xlims = xlims, ylims = ylims, /device, fill = fil, _extra = _e

    return
end
