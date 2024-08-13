; Creates Figure S3 from Jones et al (2008), using Equations S6 and S8
plot, [.1,100], 3.33e-4/[.1,100], /xs, /ys, /xlog, /ylog, yr=[3.6e-6,3.6e-3], $
      xtit='Grain size (cm)', ytit='Edge-on optical depth'
oplot, [.1,100], 2.75e-4/[.1,100], l=1
oplot, [.1,100], [1,1]*1e-5, l=2
oplot, [.1,100], [1,1]*1e-4, l=2

end
