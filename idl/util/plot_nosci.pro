; Duplicates the functionality of IDL's PLOT procedure, except tick labels are
; always in float format (NOT scientific notation), and always have enough
; decimal places to show the difference between labels.
; 
; This differs from IDL default in the following ways:
; - When the indices are less than 1 and the interval between them is in the 
;   fifth decimal place, IDL default switches to scientific notation.
; - When the indices are greater than 10 and the interval between them is in 
;   the sixth significant figure, IDL default switches to scientific notation.
; - When the indices are greater than 1 and the interval between them is in 
;   the seventh decimal place, IDL default shows only six decimal places.
; 
; Written by Matt Tiscareno, 18 November 2005

pro plot_nosci, x, y, isotropic=isotropic, max_value=max_value, min_value=min_value, nsum=nsum, polar=polar, thick=thick, xlog=xlog, ylog=ylog, ynozero=ynozero, background=background, charsize=charsize, charthick=charthick, clip=clip, color=color, data=data, device=device, normal=normal, font=font, linestyle=linestyle, noclip=noclip, nodata=nodata, noerase=noerase, position=position, psym=psym, subtitle=subtitle, symsize=symsize, t3d=t3d, ticklen=ticklen, title=title, xcharsize=xcharsize, xgridstyle=xgridstyle, xmargin=xmargin, xminor=xminor, xrange=xrange, xstyle=xstyle, xthick=xthick, xtick_get=xtick_get, xtickformat=xtickformat, xtickinterval=xtickinterval, xticklayout=xticklayout, xticklen=xticklen, xtickname=xtickname, xticks=xticks, xtickunits=xtickunits, xtickv=xtickv, xtitle=xtitle, ycharsize=ycharsize, ygridstyle=ygridstyle, ymargin=ymargin, yminor=yminor, yrange=yrange, ystyle=ystyle, ythick=ythick, ytick_get=ytick_get, ytickformat=ytickformat, ytickinterval=ytickinterval, yticklayout=yticklayout, yticklen=yticklen, ytickname=ytickname, yticks=yticks, ytickunits=ytickunits, ytickv=ytickv, ytitle=ytitle, xonly=xonly, yonly=yonly, debug=debug

if keyword_set(y) then begin
plot, x, y, isotropic=isotropic, max_value=max_value, min_value=min_value, nsum=nsum, polar=polar, thick=thick, xlog=xlog, ylog=ylog, ynozero=ynozero, background=background, charsize=charsize, charthick=charthick, clip=clip, color=color, data=data, device=device, normal=normal, font=font, linestyle=linestyle, noclip=noclip, nodata=nodata, noerase=noerase, position=position, psym=psym, subtitle=subtitle, symsize=symsize, t3d=t3d, ticklen=ticklen, title=title, xcharsize=xcharsize, xgridstyle=xgridstyle, xmargin=xmargin, xminor=xminor, xrange=xrange, xstyle=xstyle, xthick=xthick, xtick_get=xtick_get, xtickformat=xtickformat, xtickinterval=xtickinterval, xticklayout=xticklayout, xticklen=xticklen, xtickname=replicate(' ',20), xticks=xticks, xtickunits=xtickunits, xtickv=xtickv, xtitle='', ycharsize=ycharsize, ygridstyle=ygridstyle, ymargin=ymargin, yminor=yminor, yrange=yrange, ystyle=ystyle, ythick=ythick, ytick_get=ytick_get, ytickformat=ytickformat, ytickinterval=ytickinterval, yticklayout=yticklayout, yticklen=yticklen, ytickname=replicate(' ',20), yticks=yticks, ytickunits=ytickunits, ytickv=ytickv, ytitle=''
endif else begin
plot, x, isotropic=isotropic, max_value=max_value, min_value=min_value, nsum=nsum, polar=polar, thick=thick, xlog=xlog, ylog=ylog, ynozero=ynozero, background=background, charsize=charsize, charthick=charthick, clip=clip, color=color, data=data, device=device, normal=normal, font=font, linestyle=linestyle, noclip=noclip, nodata=nodata, noerase=noerase, position=position, psym=psym, subtitle=subtitle, symsize=symsize, t3d=t3d, ticklen=ticklen, title=title, xcharsize=xcharsize, xgridstyle=xgridstyle, xmargin=xmargin, xminor=xminor, xrange=xrange, xstyle=xstyle, xthick=xthick, xtick_get=xtick_get, xtickformat=xtickformat, xtickinterval=xtickinterval, xticklayout=xticklayout, xticklen=xticklen, xtickname=replicate(' ',20), xticks=xticks, xtickunits=xtickunits, xtickv=xtickv, xtitle='', ycharsize=ycharsize, ygridstyle=ygridstyle, ymargin=ymargin, yminor=yminor, yrange=yrange, ystyle=ystyle, ythick=ythick, ytick_get=ytick_get, ytickformat=ytickformat, ytickinterval=ytickinterval, yticklayout=yticklayout, yticklen=yticklen, ytickname=replicate(' ',20), yticks=yticks, ytickunits=ytickunits, ytickv=ytickv, ytitle=''
endelse

dx = abs(xtick_get[1]-xtick_get[0])
; # of decimal places is negative log of dx, rounding up
xdecimals = ceil( -alog10(dx) )
; # of leading digits is log of largest value, rounding up (at least 1)
maxx = max(abs( xtick_get ))
xlead = ceil( alog10(maxx) )>1
; If any indices are less than zero, then leave room for the "-" sign
foo = where( xtick_get lt 0, count )
if count gt 0 then xlead = xlead + 1
if xdecimals le 0 then begin
  xtickformat = '(I'+strtrim(xlead+1,2)+')'
endif else if keyword_set(yonly) then begin
  ; Force scientific notation
  xtickformat = '(E'+strtrim(xdecimals+3,2)+'.'+strtrim(xdecimals,2)+')'
endif else begin
  xtickformat = '(F'+strtrim(xlead+xdecimals+1,2)+'.'+strtrim(xdecimals,2)+')'
endelse
if keyword_set(xtickname) then begin
  nxtn = n_elements(xtickname)
  nxtg = n_elements(xtick_get)
  if nxtn lt nxtg then begin
    xtickname = [ xtickname, strarr(nxtg-nxtn) ]
    xtickname[nxtn:nxtg-1] = string( xtick_get[nxtn:nxtg-1], fo=xtickformat )
  endif
  xtickformat = ''
endif

dy = abs(ytick_get[1]-ytick_get[0])
; # of decimal places is negative log of dy, rounding up
ydecimals = ceil( -alog10(dy) )
; # of leading digits is log of largest value, rounding up (at least 1)
maxy = max(abs( ytick_get ))
ylead = ceil( alog10(maxy) )>1
; If any indices are less than zero, then leave room for the "-" sign
foo = where( ytick_get lt 0, count )
if count gt 0 then ylead = ylead + 1
if ydecimals le 0 then begin
  ytickformat = '(I'+strtrim(ylead+1,2)+')'
endif else if keyword_set(xonly) then begin
  ; Force scientific notation
  ytickname = strarr(n_elements(ytick_get))
  for j=0,n_elements(ytick_get)-1 do begin
    ytickname[j] = strsn( ytick_get[j], ndec=1, /d2, /show1 )
  endfor
endif else begin
  ytickformat = '(F'+strtrim(ylead+ydecimals+1,2)+'.'+strtrim(ydecimals,2)+')'
endelse
if keyword_set(ytickname) then begin
  nytn = n_elements(ytickname)
  nytg = n_elements(ytick_get)
  if nytn lt nytg then begin
    ytickname = [ ytickname, strarr(nytg-nytn) ]
    ytickname[nytn:nytg-1] = string( ytick_get[nytn:nytg-1], fo=ytickformat )
  endif
  ytickformat = ''
endif

if keyword_set(xstyle) then begin
  if xstyle mod 2 eq 1 then _xstyle=xstyle else _xstyle=xstyle+1
endif else _xstyle = 1
if keyword_set(ystyle) then begin
  if ystyle mod 2 eq 1 then _ystyle=ystyle else _ystyle=ystyle+1
endif else _ystyle = 1

if keyword_set(debug) then stop

axis, xaxis=0, xlog=xlog, charsize=charsize, charthick=charthick, color=color, data=data, device=device, normal=normal, font=font, subtitle=subtitle, t3d=t3d, ticklen=ticklen, xcharsize=xcharsize, xgridstyle=xgridstyle, xmargin=xmargin, xminor=xminor, xrange=!x.range, xstyle=_xstyle, xthick=xthick, xtickformat=xtickformat, xtickinterval=xtickinterval, xticklayout=xticklayout, xticklen=xticklen, xtickname=xtickname, xticks=xticks, xtickunits=xtickunits, xtickv=xtick_get, xtitle=xtitle

axis, yaxis=0, ylog=ylog, ynozero=ynozero, charsize=charsize, charthick=charthick, color=color, data=data, device=device, normal=normal, font=font, subtitle=subtitle, t3d=t3d, ticklen=ticklen, ycharsize=ycharsize, ygridstyle=ygridstyle, ymargin=ymargin, yminor=yminor, yrange=!y.crange, ystyle=_ystyle, ythick=ythick, ytickformat=ytickformat, ytickinterval=ytickinterval, yticklayout=yticklayout, yticklen=yticklen, ytickname=ytickname, yticks=yticks, ytickunits=ytickunits, ytickv=ytick_get, ytitle=ytitle

end

