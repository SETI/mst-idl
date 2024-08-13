Pro Plvar_Keep, action = what, reset = rs

;+
; NAME:
;	PLVAR_KEEP
; VERSION:
;	3.0
; PURPOSE:
;	Saves or restores the values of system variables.  
; CATEGORY:
;	Input/output.
; CALLING SEQUENCE:
;	PLVAR_KEEP, ACTION = ACT [, /RESET]
; INPUTS:
;	None.
; OPTIONAL INPUT PARAMETERS:
;	None.
; KEYWORD PARAMETERS:
;    ACTION
;	Specifies the action to take.  Two possible values, 'SAVE' and 
;	'RESTORE' (only first three characters count).
;    /RESET
;	Switch.  Protection bypass.  Normally multiple calls to PLVAR_KEEP are 
;	counted but no action is taken after the first.  For example, if 3 
;	consecutive calls with ACTION = 'SAVE' were issued then the system 
;	variables are saved on the first call and nothing happens on the next 
;	two.  Then, when calls with ACTION = 'RESTORE' are issued, only the 
;	third call will have an effect.  This mechanism allows using 'SAVE' and
;	'RESTORE' calls as pairs of braces around program sections.  For the 
;	(rare) cases when one wants to save or restore regardless of previously
;	issued calls, setting RESET disables the protection mechanism.
; OUTPUTS:
;	None.
; OPTIONAL OUTPUT PARAMETERS:
;	None.
; COMMON BLOCKS:
;	M_PLVARS
; SIDE EFFECTS:
;	None when used properly.  If the numbers of SAVEs and RESTOREs are not
;	equal, either due to an omission or to an error in a called routine, 
;	the settings won't be restored to their initial state.  In this case 
;	the cure is to execute 
;		PLVAR_KEEP, ACTION = 'RESTORE', /RESET
; RESTRICTIONS:
;	None.
; PROCEDURE:
;	Uses a common block (M_PLVARS) to save and restore the values of:
;	!P, !X, !Y, !Z, !D.NAME.  Also uses the routines DEFAULT, TYPE and
;	STRMATCH from MIDL.
; MODIFICATION HISTORY:
;	Created by 15-JUL-1991 by Mati Meron.
;-

    common m_plvars, devnam, pbak, xbak, ybak, zbak, savflag

    on_error, 1
    if Type(what) ne 7 then message, 'No action specified!'
    posib = ['SAVE', 'RESTORE']
    nwhat = StrMatch(what,posib,3)
    if nwhat eq -1 then message, 'Unknown Action!'
    savflag = Default(savflag,0)
    if keyword_set(rs) then savflag = nwhat

    case nwhat of
	0 : begin
		if savflag eq 0 then begin
		    devnam = !d.name
		    pbak = !p
		    xbak = !x
		    ybak = !y
		    zbak = !z
		endif
		savflag = savflag + 1
	    end
	1 : begin
		savflag = savflag - 1
		if savflag eq 0 then begin
		    set_plot, devnam
		    !p = pbak
		    !x = xbak
		    !y = ybak
		    !z = zbak
		endif
	    end
    endcase

    savflag = savflag > 0
    return
end
