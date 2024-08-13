common fdensity_wave11, m, phi, rsat, thoukm, rres
restore,'stretch.sav'
image_name=filenames[28]
@caviar
radscan_xr=[940,1050]
wdelete,1
wdelete,2
.run plot_radscan
moon = 1
m = 9
sat = 616l
.run get_sat_coords
phi = m * ( lcen[28] - sat_polar[1] )*!dpi/180
phi = phi + !pi
window,1
deltar = 10
.run density_wave1
x1 = 491 & x2 = 600;861
r = radi[x1:x2] - thoukm
tau_norm = val[x1:x2]/.0292
plot, r, tau_norm, /xs, /ys
.run density_wave_fit1
fdensity_wave1, r, aa1, out1
oplot, r, out1, co=ctgreen()
oplot, r, yfit, co=ctyellow()
wdelete, 2
