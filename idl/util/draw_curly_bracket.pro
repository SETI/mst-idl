pro draw_curly_bracket, xlims, ylims, vertical=vertical, noplot=noplot, $
                        xx=xx, yy=yy, color=color, xlog=xlog, ylog=ylog, $
                        noclip=noclip, dx=dx;, dxfac=dxfac

if keyword_set(xlog) then xlims=alog10(xlims)
if keyword_set(ylog) then ylims=alog10(ylims)
xhalf = mean(xlims)
yhalf = mean(ylims)
if not keyword_set(dx) then dx = (xlims[1]-xlims[0])/4
if keyword_set(dxfac) then dx = dx*dxfac
dy = (ylims[1]-ylims[0])/2
xcurve = sin( findgen(91)*!pi/180 )
ycurve = cos( findgen(91)*!pi/180 )
xx = [ xlims[0]+reverse(1-xcurve)*dx, xhalf+(xcurve-1)*dx, $
       xhalf+reverse(1-xcurve)*dx, xlims[1]-(1-xcurve)*dx ]
yy = [ yhalf-reverse(1-ycurve)*dy, yhalf+(1-ycurve)*dy, $
       yhalf+reverse(1-ycurve)*dy, yhalf-(1-ycurve)*dy ]

if keyword_set(xlog) then xx = 10^xx
if keyword_set(ylog) then yy = 10^yy
if keyword_set(vertical) then begin
  temp = xx
  xx = yy
  yy = temp
endif
if not keyword_set(noplot) then oplot, xx, yy, color=color, noclip=noclip

end
