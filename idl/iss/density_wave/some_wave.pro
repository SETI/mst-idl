common fdensity_wave33, which_params, m, lambda, lambdasat, rsat, thoukm, $
        rres, rr, yy, rres1, sigma1

radi = findgen(1000)/10
rr = radi
thoukm = 128000
yy = rr*0
m = 9
which_params = [ 1, 1, 1, 1, 0, 1 ]
pp = [ .001, 4.38e11, 128000, 0, 0, 7.5 ]

foo = fdensity_wave3( pp, f=f )
plot, rr, f
val = f

save, radi, val, thoukm, filename='some_wave.sav'

end

