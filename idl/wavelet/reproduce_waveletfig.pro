; First do .run wavetest

; To reproduce Figure 3 in Torrence and Compo:
if !p.font eq -1 then sigma = '!7r!3' else sigma = '!Ms';'sigma'
run_fft, sst, power, f, dx=dt
variance = stddev(sst)^2
plot, 1./f, power*n/2/variance, /xlog, xr=[64,.5], $
        ytit='Variance ('+sigma+'!U2!N)', xtit='Period (years)'
foo = wave_signif( sst, dt, scale, 1, lag1=lag1, dof=dof, mother=mother, $
                   cdelta=cdelta, psi0=psi0, fft_theor=fft_theor )
fft_theor = fft_theor/variance
oplot, period, fft_theor, l=1
oplot, period, fft_theor*chisqr_cvf(.05,2)/2, l=1

end
