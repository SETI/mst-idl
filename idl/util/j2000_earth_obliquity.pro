function j2000_earth_obliquity, quiet=quiet, complement=complement

; Compares outputs from SPICE (z-axis is Earth's rotational pole) with outputs
; from JPL Horizons (z-axis is Earth's orbital pole) to find the angle of
; conversion from one to another (a rotation about the x-axis with magnitude
; equal to Earth's obliquity).  

planet = 3
j2000 = 2451545.0d
cspice_furnsh, '/home/borogove/iss/NAIF/spk/de405s.bsp'
planet = [ 3, 3, 6 ]
epoch = j2000 + [ 0, 90, 0 ]
_x1 = [ -2.650257744155076E+07, -1.466873716016214E+08, 9.583854950782003E+08 ]
_y1 = [ 1.446939555203687E+08, -2.872875377422616E+07, 9.828565045290189E+08 ]
_z1 = [ -1.715011610537767E+02, 2.107978724651039E+02, -5.521277952518207E+07 ]
if keyword_set(complement) then begin
  theta = (dindgen(1000001)/10000+260) * !dpi/180
endif else begin
  theta = (dindgen(1000001)/10000) * !dpi/180
endelse
theta_calc = 0.0d0
for j=0,2 do begin
  y1 = _y1[j]
  z1 = _z1[j]
  cspice_spkez, planet[j], (epoch[j]-j2000)*86400, 'J2000', 'NONE', 10, elems, ltime
  y2=elems[1]
  z2=elems[2]
  if abs((elems[0]-_x1[j])/elems[0]) gt 1e-5 then stop
  if keyword_set(complement) then q=-1 else q=1
  theta1 = interpol( theta*180/!dpi, y1*cos(theta)-z1*q*sin(theta)-y2, 0 )
  theta2 = interpol( theta*180/!dpi, y1*q*sin(theta)+z1*cos(theta)-z2, 0 )
  theta_calc = [ theta_calc, theta1, theta2 ]
endfor
theta_calc = clip(theta_calc)
meantheta = mean(theta_calc)
sigmatheta = stddev(theta_calc) / sqrt(n_elements(theta_calc))
if not keyword_set(quiet) then print, string(meantheta,fo='(F15.10)')+' +- '+$
  string(sigmatheta,fo='(F15.10)')

return, meantheta

end
