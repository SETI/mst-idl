restore, 'spreadsheet.sav'
restore, 'stretch.sav'

foo = where( strmid(filenames,0,13) ne data[0,*], count )
if count ne 0 then stop

exposure = double(exposure_duration) / 1000

nf = n_elements(filenames)
filtername = strmid( filenames, 0, 1 )
foo = where( filtername eq 'N', count )
if count gt 0 then filtername[foo] = 'G'
foo = where( filtername eq 'W', count )
if count gt 0 then filtername[foo] = 'E'
filtername = filtername + ' Ring - ' + data[4,*] + '/' + data[5,*]
cl1 = strpos( filtername, 'CL1' )
cl2 = strpos( filtername, 'CL2' )

foo = where( cl1 ne -1 and cl2 eq -1, count )
if count gt 0 then for j=0,count-1 do begin
  filtername[foo[j]] = strmid( filtername[foo[j]], 0, cl1[foo[j]] ) + $
                      strmid( filtername[foo[j]], cl1[foo[j]]+4, 100 )
endfor
foo = where( cl1 eq -1 and cl2 ne -1, count )
if count gt 0 then for j=0,count-1 do begin
  filtername[foo[j]] = strmid( filtername[foo[j]], 0, cl2[foo[j]]-1 )
endfor

dir = '/home/borogove/iss/caviar/developmental/';'~/idl/iss/diffring_exposures/'
restore, dir + 'ioverffac.sav'
jj = intarr(nf)
for j=0,nf-1 do jj[j] = where( filtertit eq filtername[j] )

sum = data[7,*]
foo = where( sum eq 'FULL', count  )
if count gt 0 then sum[foo] = '1'
foo = where( sum eq 'SUM2', count  )
if count gt 0 then sum[foo] = '2'
foo = where( sum eq 'SUM4', count  )
if count gt 0 then sum[foo] = '4'
sum = fix(sum)
gain = fix(data[6,*])

run_calculate_exposures, exposure, frac, $
  phase=_keywords.ringplane_aimpoint_phase_angle, ringfrac=ringfrac, $
  bgfrac=bgfrac, filtername=filtername, jj=jj, $
  noplot=noplot, gain=gain, sum=sum, debug=debug, $
  elevation=90-_keywords.ringplane_aimpoint_emission_angle

;print, rotate([ [_keywords.ringplane_aimpoint_phase_angle], $
;                [90-_keywords.ringplane_aimpoint_emission_angle], $
;                [ringfrac], [bgfrac], [frac] ],4)

end
