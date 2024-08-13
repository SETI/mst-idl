nbfig = 1
!p.multi=[0,3,3]

; Atlas 5:4
restore, '~/Data/images/SOI/SOISPTURN/atlas54.sav'
lambda=318.91
lambdasat=29.51
rsat=137665.34
run_wavelet, r, tau, minlam=.8, noplot=nbfig, ytit='Optical Depth'
fit_wavelet, dolzr=dolzr, rsat=rsat, lambdapic=lambda, lambdasat=lambdasat, mm=5, xx0=300, xx1=347, xr=[855,880], cutoff=8, rrshift=15, p0 = [ .00154823, 7.22363 ], /flip, nbfig=nbfig, resname='Atlas 5:4 (Cassini Division):!C'

; Pan 7:6
restore, '~/Data/images/SOI/SOISPTURN/pan76.sav'
lambda=319.73
lambdasat=-24.01
rsat=133584.59
run_wavelet, r, tau, minlam=.8, noplot=nbfig, ytit='Optical Depth'
fit_wavelet, dolzr=dolzr, rsat=rsat, lambdapic=lambda, lambdasat=lambdasat, mm=7, cutoff=8, xx0=45, xx1=120, xr=[692,728], rrshift=0, p0 = [ .0027862847, 7.6624201 ], /flip, nbfig=nbfig, resname='Pan 7:6 (Cassini Division):!C'

; Prometheus 9:8
restore, '~/Data/images/SOI/SOISPTURN/prom98.sav'
lambda=323.45
lambdasat=6.30
rsat=139378.07
run_wavelet, r, tau, minlam=2, noplot=nbfig, ytit='Optical Depth'
fit_wavelet, dolzr=dolzr, rsat=rsat, lambdapic=lambda, lambdasat=lambdasat, mm=9, cutoff=1000, xx0=250, xx1=550, xr=[920,1070], rrshift=0, p0 = [ .0466609, 8.79864 ], noise=[700,1048], nbfig=nbfig, resname='Prometheus 9:8 (A Ring):!C'

plots,/device,[-.04,1.04]*!d.x_size,[.33333,.33333]*!d.y_size
plots,/device,[-.04,1.04]*!d.x_size,[.33333,.33333]*!d.y_size*2
