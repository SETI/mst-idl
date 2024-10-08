function get_mooncolor, ring_rads_legend, nocolor=nocolor, ct=ct

nrrl = n_elements(ring_rads_legend)
clr = lonarr(nrrl)
for j=0,nrrl-1 do begin
  moon = strmid( ring_rads_legend[j], 0, 2 )
  if !d.name eq 'X' then begin
    if keyword_set(ct) then clr[j]=ctwhite() else clr[j]=white()
  endif else clr[j]=0
  redo:
  if not keyword_set(nocolor) then case moon of 
    'Pa':  if keyword_set(ct) then clr[j]=ctred() else clr[j]=red()
    'Pn':  if keyword_set(ct) then clr[j]=ctred() else clr[j]=red()
    'Pr':  if keyword_set(ct) then clr[j]=ctcyan() else clr[j]=cyan()
    'Pd':  if keyword_set(ct) then clr[j]=ctblue() else clr[j]=blue()
    'At':  if keyword_set(ct) then clr[j]=ctgreen() else clr[j]=green()
    'Ja':  if keyword_set(ct) then clr[j]=ctorange() else clr[j]=orange()
    'Ep':  if keyword_set(ct) then clr[j]=ctorange() else clr[j]=orange()
    'Mi':  if keyword_set(ct) then clr[j]=ctpurple() else clr[j]=purple()
    'En':  if keyword_set(ct) then clr[j]=ctpurple() else clr[j]=purple()
    'Te':  if keyword_set(ct) then clr[j]=ctpurple() else clr[j]=purple()
    'Ti':  if keyword_set(ct) then clr[j]=ctgreen() else clr[j]=green()
    'Hy':  if keyword_set(ct) then clr[j]=ctgreen() else clr[j]=green()
    'Ia':  if keyword_set(ct) then clr[j]=ctgreen() else clr[j]=green()
    '!C':  begin
             moon = strmid( ring_rads_legend[j], 2, 2 )
             goto, redo
           end
    else:
  endcase
endfor

return, clr

end
