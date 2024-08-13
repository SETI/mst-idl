if not keyword_set(cray_thresh) then cray_thresh = max(im[2:nl-3,2:nl-3])
if keyword_set(cray_zero) then begin
  cray = wher( rawim eq 0, count )
endif else begin
  cray = wher( im ge cray_thresh, count )
endelse
if count eq 0 then cray = replicate(0,1,2) else cray = rotate( cray, 3 )
foo = where( cray[*,0] gt 1 and cray[*,0] lt nl-2 and cray[*,1] gt 1 and cray[*,1] lt nl-2, count )
if count eq 0 then cray = 0 else cray = cray[foo,*]
@plot_cray

end
