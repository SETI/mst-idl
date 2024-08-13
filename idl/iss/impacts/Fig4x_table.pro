;paperplot = 1
;.run impact_image_coverage_area
;print, location
;C C C Bq Cq Cq Aq C A
;print, ea
;;    0.0230000    0.0450000    0.0600000     0.150000     0.430000     0.120000      2.10000    0.0670000    0.0670000
;print, ea^3*4*!pi/3*1000*1e4
;;      509.650      3817.04      9047.79      141372.  3.33038e+06      72382.3  3.87924e+08      12598.3      12598.3
;print, ea3
;;     0.110000     0.220000     0.300000     0.780000      2.30000     0.620000      13.9000     0.330000     0.330000
;print, ea3^3*4*!pi/3*1000*1e4
;;      55752.8      446022.  1.13097e+06  1.98780e+07  5.09650e+08  9.98306e+06  1.12495e+11  1.50533e+06  1.50533e+06
features = [ 'C1--C5', 'C1--C2', 'C1', 'Bx', 'Cx (central)', 'Cx (peripheral)', 'Ax', 'C6', 'A (non-detection)' ]
nn = n_elements(features)
orbit = [ '007', '007', '007', '116', '116', '116', '116', '169', '169' ]
mcloud4 = [ 500., 3800, 12000, 140000, 3.3e6, 7.2e4, 3.8e8, 13000, 13000 ]
mcloud4log = floor(alog10(mcloud4))
mcloud4txt = strarr(nn)
for j=0,nn-1 do mcloud4txt[j] = string( mcloud4[j]/10.^mcloud4log[j], fo='(F3.1)' ) + ' \times 10^{' + strtrim(mcloud4log[j],2) + '}'
rimp4 = [ '0.023', '0.045', '0.065', '0.15', '0.43', '0.12', '2.1', '0.067', '0.067' ]
mcloud3 = [ 5.6e4, 4.5e5, 1.5e6, 2.0e7, 5.1e8, 1.0e7, 1.1e11, 1.5e6, 1.5e6 ]
mcloud3log = floor(alog10(mcloud3))
mcloud3txt = strarr(nn)
for j=0,nn-1 do mcloud3txt[j] = string( mcloud3[j]/10.^mcloud3log[j], fo='(F3.1)' ) + ' \times 10^{' + strtrim(mcloud3log[j],2) + '}'
rimp3 = [ '0.11', '0.22', '0.33', '0.78', '2.3', '0.62', '13.9', '0.33', '0.33' ]
for j=0,nn-1 do begin &$
  print, features[j] + ' & ' + orbit[j] + ' & $' + mcloud3txt[j] + '$ & ' + rimp3[j] + ' & $' + $
         mcloud4txt[j] + '$ & ' + rimp4[j] + '\\' &$
endfor

