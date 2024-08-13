paperplot = 1
.run impact_image_coverage_area
;print, location
;C C C Bq Cq Cq Aq C A
;; Reproduce coverage fractions in Table S5
;print, 1/expnum / totalarea[[0,0,0,1,0,2,2,0,2]]/1e6 / age * 100 * [5,2,1,1,1,1,1,1,1]
;;      0.52505427      0.52505427      0.52505427       12.146282       1.4028627       1.4028627       5.2641738       1.5668671      0.79464142
;print, age / 3600
;      1.00000      1.00000      1.00000      4.00000      30.0000      12.9500      23.5000     0.860000     0.860000
;print, expnum
;   2.8919291e-17   1.1567716e-17   5.7838582e-18   3.4042469e-20   7.2158155e-20   4.4653461e-19   6.5575555e-20   2.2536747e-18   1.1870538e-17
features = [ 'C1--C5', 'C1--C2', 'C1', 'Bx', 'Cx (central)', 'Cx (peripheral)', 'Ax', 'C6', 'A (non-detection)' ]
nn = n_elements(features)
orbit = [ '007', '007', '007', '116', '116', '116', '116', '169', '169' ]
num = [ 5, 2, 1, 1, 1, 1, 1, 1, 1 ]
numtxt = [ '5', '2', '1', '1', '1', '1', '1', '1', '0' ]
areal_coverage = 1/expnum / 1e6 / age * [5,2,1,1,1,1,1,1,1]
areal_coverage_log = floor(alog10(areal_coverage))
areal_coverage_txt = strarr(nn)
for j=0,nn-1 do areal_coverage_txt[j] = string( areal_coverage[j]/10.^areal_coverage_log[j], fo='(F3.1)' ) + $
   ' \times 10^{' + strtrim(areal_coverage_log[j],2) + '}'
agetxt = [ '1', '1', '1', '4', '30', '13', '23.5', '0.86', '0.86' ]
flux = num / areal_coverage / 1e6 / age
flux_log = floor(alog10(flux))
flux_txt = strarr(nn)
for j=0,nn-1 do flux_txt[j] = string( flux[j]/10.^flux_log[j], fo='(F3.1)' ) + $
   ' \times 10^{' + strtrim(flux_log[j],2) + '}'
for j=0,nn-1 do begin &$
  print, features[j] + ' & ' + orbit[j] + ' & ' + numtxt[j] + ' & $' + areal_coverage_txt[j] + '$ & ' + agetxt[j] + ' & $' + $
         flux_txt[j] + '$ \\' &$
endfor

