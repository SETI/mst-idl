function redge_prune, redge, nn, option=option, debug=debug

if n_params() eq 0 then begin
  print, 'Syntax:  redge_out = REDGE_PRUNE( redge_in, [nn] )'
  print, 'Removes bad points from a 2xn redge array.'
  print, 'Option 1: Remove all points above or below an input value.'
  print, 'Option 2: Remove points selected by cursor.'
  print, 'Option 3: Remove points specified in input nn.'
  stop
endif

again:
plot_nosci, redge[0,*], tkm(redge[1,*]), /xs, /ys, ps=4
if not keyword_set(option) then begin
  if keyword_exists(nn) then option = 3 else begin
    reply = ''
    while reply eq '' do begin
      print, 'Select an option (or "q" to quit):'
      print, '1: Remove all points above or below an input value.'
      print, '2: Remove points selected by cursor.'
      read, reply
      case reply of
        '1': option = 1
        '2': option = 2
        'q': retall
        else: reply = ''
      endcase
    endwhile
  endelse
endif

case option of
  '1': begin
    option = 1
    reply = ''
    print, 'Enter cutoff value (negative to cut off points below):'
    read, reply
    if reply eq '' or reply eq '-' then begin
      print, 'Click at cutoff value:'
      cursor, x, y, 1, /data
      if reply eq '-' then begin
        reply = -y
        abbel = 'below '
      endif else begin
        reply = y
        abbel = 'above '
      endelse
      count = 1
      print, 'Cutting off points '+abbel+strtrim(abs(reply),2)
    endif
    if reply lt 0 then nn = where( tkm(redge[1,*]) lt abs(reply), count )
    if reply gt 0 then nn = where( tkm(redge[1,*]) gt reply, count )
  end
  '2': begin
    option = 2
    print, 'Click on point to remove:'
    cursor, x, y, 1, /data
    dist = (redge[0,*]-x)^2 / (!x.crange[1]-!x.crange[0])^2 + $
           (tkm(redge[1,*])-y)^2 / (!y.crange[1]-!y.crange[0])^2
    nn = where( dist eq min(dist), count )
  end
  '3': begin
    if not keyword_exists(nn) then begin
      stop, 'Option 3 selected, but no nn specified.'
    endif else count = n_elements(nn)
  end
  else: stop
endcase
if count gt 0 then begin
  oplot, [redge[0,nn]], tkm([redge[1,nn]]), ps=4, co=red()
endif else begin
  print, 'Points to be removed contains no members.  Trying again...'
  goto, again
endelse

reply = ''
while reply eq '' do begin
  print, 'Remove points shown in red?  (y/n/q)'
  read, reply
  case reply of
    'n': goto, again
    'q': retall
    else: if reply ne 'y' then reply = ''
  endcase
endwhile

redge_out = redge[*,vec_remove( lindgen(n_elements(redge[0,*])), nn )]
if keyword_set(debug) then stop
return, redge_out

end

