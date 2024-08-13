whizin_files = [ 'Pandora_ILR_1.dat', 'Pandora_ILR_2.dat', $
                 'Prom_OLR_1.dat', 'Prom_OLR_2.dat' ]
nwf = n_elements(whizin_files)
nn = 150;300
res_descrip = strarr(nn,nwf)
r_whizin = fltarr(nn,nwf)
r_tiscareno = fltarr(nn,nwf)
for k=0,nwf-1 do begin
  openr, 1, whizin_files[k]
  aa = ''
  readf, 1, aa
  readf, 1, aa
  if k le 1 then moon=617l else moon=616l
  for j=0l,nn-1 do begin
    readf, 1, aa
    bb = strsplit(aa,/extract)
    r_whizin[j,k] = bb[2]*60330.0
    if k le 1 then begin
      rl1 = fix(bb[0])
      rl2 = fix(bb[1])
    endif else begin
      rl1 = fix(bb[1])
      rl2 = fix(bb[0])
      if k eq 2 then begin
        rl1 = rl1 + 1
        rl2 = rl2 + 1
      endif 
    endelse 
    r_tiscareno[j,k] = resloc( rl1, rl2, moon, gmsource='whizin11', $
                             moonsource='spitale06', res_descrip=_res_descrip )
    ;r_tiscareno[j,k] = resloc( rl1, rl2, moon, res_descrip=_res_descrip )
    res_descrip[j,k] = _res_descrip
  endfor
  close, 1
endfor

if keyword_set(dolzr) then begin
  lzr, 'compare_whizin'
  @plot_prepare
endif

!P.multi = [0,2,2]
;for k=0,3 do plot, r_whizin[*,k]-r_tiscareno[*,k], xtit='m-1', $
;                   ytit='Residual (km)'
plot, [0,nn], [min(r_whizin-r_tiscareno),max(r_whizin-r_tiscareno)], /xs, /ys, $
               /nodata, xtit='m-1', ytit='Residual (km)'
for k=0,3 do begin
  oplot, r_whizin[*,k]-r_tiscareno[*,k], l=k
  oplot, [80,95], 23.3-k*2.5+[0,0], l=k
  xyouts, 100, 23-k*2.5, whizin_files[k]
endfor

if keyword_set(dolzr) then clzr

end
