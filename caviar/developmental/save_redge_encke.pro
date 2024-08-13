;  This code is to allow for distinction between various saved edges from the same picture
;  This code is not generally useful, but only for saving .edge files for use in bootstrap

period = rstrpos( image_name, '.' )
filestem = strmid( image_name, 0, period )

if edge eq 1 and (gap eq 'E' or gap eq 'e') then savefile = filestem+'.edgeEI' 
if edge eq 2 and (gap eq 'E' or gap eq 'e') then savefile = filestem+'.edgeEO'

if edge eq 1 and (gap eq 'K' or gap eq 'k') then savefile = filestem+'.edgeKI' 
if edge eq 2 and (gap eq 'K' or gap eq 'k') then savefile = filestem+'.edgeKO'

if (gap eq 'R' or gap eq 'r') then savefile = filestem+'.edgeR'

print, 'Saving edgefit information to '+savefile
redge_cmat = cmat
save, redge, redge_sigma, redge_cmat, filename=savefile

end
