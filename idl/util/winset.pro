pro winset, num, nowin

if n_params() eq 0 then begin
  print, 'Syntax:  WINSET, num, nowin'
  print, 'If a window #num exists, make it the active window.  If not, returns nowin=1.'
  print, 'Note that, if a catch system has been set up elsewhere in the code, this'
  print, 'routine will deactivate it.'
  retall
endif

catch, error_status
if error_status eq 0 then begin
  nowin=0
  wset, num
endif else begin
  nowin=1
endelse
catch, /cancel

end
