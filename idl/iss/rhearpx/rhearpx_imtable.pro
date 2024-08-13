; .run rhea_reproject_display

foo = where( strlen(spreadsheet[0,*]) ne 13, count )
if count ne 0 then stop
foo = where( spreadsheet[0,*] ne strmid(filenames,0,13), count )
if count ne 0 then stop
nf = n_elements(filenames)
if n_elements(spreadsheet[0,*]) ne nf then stop
grp = strarr(nf)
for j=0,ngrp1-1 do grp[lims1[0,j]:lims1[1,j]] = groupshort[j]

for j=0,nf-1 do begin
  if grp[j] eq '100' then num='2' else num='1'
  fo = '("' + grp[j] + ' & ' + strmid(filenames[j],0,11) + ' & ' + $
       strmid(spreadsheet[2,j],0,4)+'-'+strmid(spreadsheet[2,j],4,3) + $
       ' & ",F5.1,"$^\circ$ & ",F5.1,"$^\circ$ & ",F5.1,"$^\circ$ & ",F5.1,'+$
       '" & ",F5.1," & ",F5.'+num+'," \\")'
  print, fo=fo, incidence[j], emission[j], phase[j], range[j], width[j], $
         rms[j]/1e-6
endfor

stop

for j=0,ngrp-1 do begin
  if strmid(group[j],0,3) eq '100' then num='2' else num='1'
  fo = '("' + strmid(group[j],0,3) + ' & ' + $
       strmid(filenames[lims[0,j]],0,11) + ' -- ' + $
       strmid(filenames[lims[1,j]],6,5) + ' & ' + $
       strmid(spreadsheet[2,lims[0,j]],0,4)+'-'+$
       strmid(spreadsheet[2,lims[1,j]],4,3) + ' & ",I2," & ",F5.1,'+$
       '"$^\circ$ & ",F5.'+num+'," \\")' 
  print, fo=fo, lims[1,j]-lims[0,j]+1, mean(phase[lims[0,j]:lims[1,j]]), $
         rms_grp[j]/1e-6
endfor

end
