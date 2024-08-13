; .run image_coverage
tit = rev+'/'+obsname
xtit = 'Radius'+tkmtit()
!p.multi = [0,2,2]
fooall = where( gridrad ge 74490 and gridrad le 136770, countall )
fooa = where( gridrad ge 122050 and gridrad le 136770, counta )
foocd = where( gridrad ge 117500 and gridrad le 122050, countcd )
foob = where( gridrad ge 91980 and gridrad le 117500, countb )
fooc = where( gridrad ge 74490 and gridrad le 91980, countc )
for q=0,1 do begin
  if q eq 0 then begin
    yy = coverage
    ytit='Fractional longitudinal coverage'
  endif else begin
    yy = coverage*2*!dpi*tkm(gridrad)
    ytit='Longitudinal coverage'+tkmtit()
  endelse 
  plot, tkm(gridrad), yy, /xs, tit=tit, xtit=xtit, ytit=ytit
  if keyword_set(counta) then oplot, tkm(gridrad[fooa]), yy[fooa], co=ctblue()
  if keyword_set(countb) then oplot, tkm(gridrad[foob]), yy[foob], co=ctgreen()
  if keyword_set(countc) then oplot, tkm(gridrad[fooc]), yy[fooc], co=ctcyan()
  if keyword_set(countcd) then oplot, tkm(gridrad[foocd]), yy[foocd], co=ctred()
endfor
dr = mean( gridrad[1:nrad-1] - gridrad[0:nrad-2] )
area = dblarr(5)
arealegend = [ 'C', 'B', 'CD', 'A', 'All' ]
for q=0,4 do begin
  case q of
    0: foo = fooc
    1: foo = foob
    2: foo = foocd
    3: foo = fooa
    4: foo = fooall
  endcase 
  case q of
    0: count = countc
    1: count = countb
    2: count = countcd
    3: count = counta
    4: count = countall
  endcase 
  if keyword_set(count) then begin
    area[q] = total(coverage[foo]*2*!dpi*gridrad[foo]) * dr
    if area[q] gt 0 then begin
      areaexp = floor( alog10(area[q]) )
      print, 'Total area covered (' + arealegend[q] + '):  ' + $
             string( area[q]/10.^areaexp, fo='(F4.2)' ) + $
             ' x 10^' + strtrim(areaexp,2) + ' km^2'
    endif
  endif
endfor

sfile = 'image_coverage_area.sav'
if keyword_set(savenew) or not keyword_set(findfile(sfile)) then begin
  save, area, arealegend, filename=sfile
  print, sfile + ' saved'
endif

end
