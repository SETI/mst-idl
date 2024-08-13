redo:
yy = 0
yyy = 0
shadow = 0
stripe = 0
drawnyet = 0

print, 'Select as many points as needed to define a curve.'
print, 'Right-click to proceed.'
!mouse.button = 0
while !mouse.button ne 4 do begin
  cursor, x, y, 3, /device
  if !mouse.button ne 4 then begin
    if not keyword_set(drawnyet) then begin
      yy = [[nl-1-y],[x]]
      drawnyet = 1
    endif else yy = [ yy, [[nl-1-y],[x]] ]
  endif
  if keyword_set(yy) then begin
    nyy = n_elements(yy[*,0])
    ;yy = yy[ sort(yy[*,0]), * ]
  endif
  if not keyword_set(noplot) then begin
    @disp
    if keyword_set(yy) then begin
      if nyy gt 1 then begin
        ; Extrapolate the beginning of the curve to x=[0,nl-1]
        end1x = interpol( yy[0:1,0], yy[0:1,1], $
                          ([0,nl-1])[yy[0,1] gt yy[1,1]] )
        ; Extrapolate the beginning of the curve to y=[0,nl-1]
        end1y = interpol( yy[0:1,1], yy[0:1,0], $
                          ([0,nl-1])[yy[0,0] gt yy[1,0]] )
        foo = where( [end1x,end1y] ge 0 and [end1x,end1y] lt nl, count )
        if count ne 1 then stop
        case foo[0] of
          0: yyy = [ [[end1x],[([0,nl-1])[yy[0,1] gt yy[1,1]]]], yy ]
          1: yyy = [ [[([0,nl-1])[yy[0,0] gt yy[1,0]]],[end1y]], yy ]
        endcase 
        ; Extrapolate the end of the curve to x=[0,nl-1]
        end2x = interpol( yy[nyy-2:nyy-1,0], yy[nyy-2:nyy-1,1], $
                          ([0,nl-1])[yy[nyy-2,1] lt yy[nyy-1,1]] )
        ; Extrapolate the end of the curve to y=[0,nl-1]
        end2y = interpol( yy[nyy-2:nyy-1,1], yy[nyy-2:nyy-1,0], $
                          ([0,nl-1])[yy[nyy-2,0] lt yy[nyy-1,0]] )
        foo = where( [end2x,end2y] ge 0 and [end2x,end2y] lt nl, count )
        if count ne 1 then stop
        case foo[0] of
          0: yyy = [ yyy, [[end2x],[([0,nl-1])[yy[nyy-2,1] lt yy[nyy-1,1]]]] ]
          1: yyy = [ yyy, [[([0,nl-1])[yy[nyy-2,0] lt yy[nyy-1,0]]],[end2y]] ]
        endcase 
      endif else yyy = yy
      nyyy = n_elements(yyy[*,0])
      plots, yyy[0:nyyy-1,1], nl-1-yyy[0:nyyy-1,0], ps=-4, color=green(), /dev
    endif
  endif
endwhile

if keyword_set(yyy) then begin
  print, 'Click which side of the line to designate as shadow.'
  cursor, x, y, 3, /device
  line = nl-1-y
  sample = x
  ; Extrapolate along x- and y-axes to the curve to see what side
  ; of it we're on.
  xcorn = ([0,nl-1])[sample gt interpol( yyy[*,1], yyy[*,0], line )]
  ycorn = ([0,nl-1])[line gt interpol( yyy[*,0], yyy[*,1], sample )]
  if ( yyy[0,0] eq 0 or yyy[0,0] eq nl-1 ) and $
     ( yyy[nyyy-1,0] eq 0 or yyy[nyyy-1,0] eq nl-1 ) then begin
    ; Line passes through both horizontal edges
    xcorn = replicate( xcorn, 2 )
    ycorn = yyy[ [nyyy-1,0], 0 ]
  endif else if ( yyy[0,1] eq 0 or yyy[0,1] eq nl-1 ) and $
     ( yyy[nyyy-1,1] eq 0 or yyy[nyyy-1,1] eq nl-1 ) then begin
    ; Line passes through both vertical edges
    xcorn = yyy[ [nyyy-1,0], 1 ]
    ycorn = replicate( ycorn, 2 )
  endif else begin
    ; Line passes through one horizontal edge and one vertical edge
    if ( xcorn eq yyy[0,1] and ycorn eq yyy[nyyy-1,0] ) or $
       ( ycorn eq yyy[0,0] and xcorn eq yyy[nyyy-1,1] ) then begin
      ; The side with only one corner has been selected
    endif else begin
      ; The side with three corners has been selected; I can't prove it, 
      ; but it seems that [xcorn,ycorn] always represent the middle corner
      if yyy[nyyy-1,0] eq 0 or yyy[nyyy-1,0] eq nl-1 then begin
        ; The last point in yyy is on the horizontal edge
        xcorn = [ xcorn, xcorn, ([0,nl-1])[xcorn eq 0] ]
        ycorn = [ ([0,nl-1])[ycorn eq 0], ycorn, ycorn ]
      endif else begin
        ; The last point in yyy is on the vertical edge
        xcorn = [ ([0,nl-1])[xcorn eq 0], xcorn, xcorn ]
        ycorn = [ ycorn, ycorn, ([0,nl-1])[ycorn eq 0] ]
      endelse 
    endelse
  endelse
  yyyy = [ yyy, [[ycorn],[xcorn]] ]
  _shadow = polyfillv( yyyy[*,1], yyyy[*,0], nl, nl )
  nshadow = n_elements(_shadow)
  dim = 2
  axes = [ nl, nl ]
  shadow = lonarr(dim,nshadow)
  num = lonarr(dim)+1
  for i=1,dim-1 do num[i:dim-1] = num[i:dim-1]*axes[i-1]
  for i=dim-1,0,-1 do begin
    rem = _shadow mod num[i]
    shadow[i,*] = (_shadow-rem)/num[i]
    _shadow = rem
  endfor
  shadow = rotate( shadow, 3 )
  if not keyword_set(noplot) then begin
    @disp
    plots, shadow[*,1], nl-1-shadow[*,0], ps=3, color=yellow(), /device
    plots, yyy[0:nyyy-1,1], nl-1-yyy[0:nyyy-1,0], ps=-4, color=green(), /device
  endif
endif

query:
reply = ''
while reply eq '' do begin
  print, '[f]inish or [r]edo or [a]dd a horizontal stripe?'
  read, reply
  case reply of
    'r':  goto, redo
    'a':  dummy = 1
    'f':  dummy = 1
    else:  reply = ''
  endcase 
endwhile
if reply eq 'a' then begin
  print, 'Click on the upper boundary of the horizontal stripe'
  cursor, x, y0, 3, /device
  print, 'Click on the lower boundary of the horizontal stripe'
  cursor, x, y1, 3, /device
  stripe = [ [replicate(nl-1-y0,nl)], [lindgen(nl)] ]
  for j=y0-1,y1,-1 do begin
    stripe = [stripe, [ [replicate(nl-1-j,nl)], [lindgen(nl)] ]]
  endfor
  if not keyword_set(noplot) then begin
    @disp
    plots, stripe[*,1], nl-1-stripe[*,0], ps=3, color=yellow(), /device
    if keyword_set(shadow) then begin
      plots, shadow[*,1], nl-1-shadow[*,0], ps=3, color=yellow(), /device
      plots, yyy[0:nyyy-1,1], nl-1-yyy[0:nyyy-1,0], ps=-4, color=green(), /dev
    endif
  endif
  goto, query
endif
if keyword_set(shadow) and keyword_set(stripe) then begin
  shadow = [ shadow, stripe ]
endif
if keyword_set(stripe) and not keyword_set(shadow) then begin
  shadow = stripe
endif
print, 'If you want radscan etc. to avoid the shadow, then type cray=shadow'

end
