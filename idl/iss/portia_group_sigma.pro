; Calculate the surface densities for moons of the Portia group 
; (if they were to be disrupted)
aa = [ 53764.0d0, 59165, 61767, 62659, 64358, 66097, 69927, 74392, 75255, $
       76416, 86004, 97736, 129847 ]
if keyword_set(portia_group) then aa = aa[0:9]
aa = aa * 1e5  ; Convert km to cm
abins = rebin([ [aa[0:n_elements(aa)-2]], [aa[1:n_elements(aa)-1]] ],$
              n_elements(aa)-1,1)
ax = aa[1:n_elements(aa)-2]
da = abins[1:n_elements(abins)-1] - abins[0:n_elements(abins)-2]
mm = [ 10.7d0, 39.5, 20.5, 64.1, 195, 29.2, .4, 41.1, 1.5, 333, 1.0 ]*1e-10
mm = mm * 8.6849e28  ; Convert M_U to grams
if keyword_set(portia_group) then mm = mm[0:7]

area = 2*!dpi*ax*da
sigma = mm / area
sigma_total = total(mm) / total(area)

end
