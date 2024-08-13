nn = 210
m = indgen(nn) + 40
rres = fltarr(nn)
lambdares = fltarr(nn)

for j=0,nn-1 do rres[j] = resloc(m[j],m[j]-1,10)
lambdares = rres[1:nn-1] - rres
rres1 = rebin( [ [rres[1:nn-1]], [rres[0:nn-2]] ], nn-1, 1 )

plot, rres1 - 130000, lambdares, /xs, /ys, xtit='Radius - 130000 km', $
	ytit='Wavelength (km)'

ntheta = 16
theta = (findgen(ntheta)+1)*45 * !pi/180
theta0 = 342.1 * !pi/180
for j=0,ntheta-1 do begin
  oplot, rres1 - 130000, 3*!pi*(rres1-133586)^2/133586/theta[j], l=1
endfor
oplot, rres1 - 130000, 3*!pi*(rres1-133586)^2/133586/theta0, l=3


end

