Pro Errbars, x, y, xerr = xer, yerr = yer, _extra = _e

;+
; NAME:
;	ERRBARS
; VEERSION:
;	3.0
; PURPOSE:
;	Overplots error bars over an existing plot.  More general than the 
;	library routines ERRPLOT and PLOTERR, since it allows to independently 
;	vary both X and Y errors, and allows for nonsymmetric error bars.
; CATEGORY:
;	Plotting.
; CALLING SEQUENCE:
;	ERRBARS, [X,] Y [, XERR = XER, YERR = YER]
; INPUTS:
;    X, Y
;	Vectors containing the data points' coordinates.  If only one is given
;	it is taken to be Y, same as in the PLOT command.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    XERR
;	Either a vector or a (2,N) array where N is the number of the data 
;	points.  Contains the X-errors.  If given as a 2 dimensional array, the
;	entries XERR(0,i) and XERR(1,i) are taken as the errors of X(i) in the
;	negative and positive directions, respectively.  If given as a vector,
;	the entry XERR(i) serves as both the negative and positive error of 
;	X(i) and therefore symmetric error bars are drawn.  If not provided,
;	the default is no X-errors.
;    YERR
;	Same as above, for the Y-errors.
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
;       The keywords passed through _EXTRA are transferred to the PLOTS 
;	routine.  No keyword verification is performed by ERRBARS.
; PROCEDURE:
;	Straightforward.  Uses DEFAULT from MIDL.
; MODIFICATION HISTORY:
;	Created 10-DEC-1991 by Mati Meron.
;       Modified 15-DEC-1993 by Mati Meron.  Now ERRBARS takes advantage of the
;       keyword inheritance property and accepts most of IDL plotting keywords.
;-

    on_error, 1
    len = n_elements(x)
    xp = x
    if n_params() eq 1 then begin
	yp = x
	xp = findgen(len)
    endif else yp = y

    wxer = Default(xer,fltarr(2,len))
    dum = size(wxer)
    if dum(0) eq 1 then wxer = transpose([[wxer],[wxer]])
    wyer = Default(yer,fltarr(2,len))
    dum = size(wyer)
    if dum(0) eq 1 then wyer = transpose([[wyer],[wyer]])

    xlh = transpose([[xp - wxer(0,*)],[xp + wxer(1,*)]])
    ylh = transpose([[yp - wyer(0,*)],[yp + wyer(1,*)]])

    for i = 0l, len - 1 do begin
	plots, xlh(*,i), [yp(i),yp(i)], _extra = _e
	plots, [xp(i),xp(i)], ylh(*,i), _extra = _e
    endfor

    return
end
