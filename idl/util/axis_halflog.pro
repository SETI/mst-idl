pro axis_halflog, xaxis=_xaxis, yaxis=_yaxis, oneaxis=_oneaxis, $
                  halflog25=halflog25, xtn=_xtn

if keyword_exists(_xaxis) then begin
  xaxis = _xaxis
  if keyword_exists(yaxis) then begin
    print, 'Setting xaxis = '+strtrim(xaxis,2)+'.  Ignoring yaxis.'
  endif
endif else begin
  if keyword_exists(_yaxis) then yaxis = _yaxis else xaxis = 0
endelse
if keyword_set(_oneaxis) then oneaxis = 1 else oneaxis = 0

if keyword_exists(xaxis) then ax = xaxis else ax = yaxis
if ax eq 0 then sn = 1 else if ax eq 1 then sn = -1 else stop, 'ax = ', ax

for j=0,(1-oneaxis)*sn,sn do begin
  if keyword_set(halflog25) then begin
    vv = [ '0.002', '0.02', '0.2', '2', '20', '200', '2000' ]
    vv = [ vv, '0.005', '0.05', '0.5', '5', '50', '500', '5000' ]
  endif else begin
    vv = [ '0.003', '0.03', '0.3', '3', '30', '300', '3000' ]
  endelse
  if keyword_exists(xaxis) then crange = !x.crange else crange = !y.crange
  vv = [ floor(crange[0]), ceil(crange[1]) ]
  ndec = -vv[0] > 0
  if keyword_set(halflog25) then begin
    vv = [ 2 * 10^( findgen(vv[1]-vv[0]) + vv[0] ), $
           5 * 10^( findgen(vv[1]-vv[0]) + vv[0] ) ]
  endif else begin
    vv = 3 * 10^( findgen(vv[1]-vv[0]) + vv[0] )
  endelse
  if j eq 0 then begin
    if ndec eq 0 then begin
      xtn = strtrim(string( vv, fo='(I40)' ),2)
    endif else begin
      xtn = strtrim(string( vv, fo='(F40.'+strtrim(ndec,2)+')' ),2)
    endelse
  endif else xtn = replicate(' ',20)
  if keyword_set(_xtn) then xtn = _xtn
  if keyword_exists(xaxis) then begin
    axis, xaxis=xaxis+j, /data, /xs, xticks=(n_elements(vv)-1)>1, $
          xtickn=xtn, xtickv=vv
  endif else begin
    axis, yaxis=yaxis+j, /data, /ys, yticks=(n_elements(vv)-1)>1, $
          ytickn=xtn, ytickv=vv
  endelse
endfor

end
