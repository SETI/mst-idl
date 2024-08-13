!p.multi = [0,1,2]
plot, tkm(radi), vfilt, xr=[131.36,131.403]
oplot, tkm(radi1), vmodel, co=green()
plot, tkm(radi1), vphase, xr=[131.36,131.403]
_vphase=vphase
run_wavelet, radi1, vmodel, vwave, /noplot
vphase = unwrap_phase(get_phase( total(vwave,2), wrap=vwrap) )
oplot, tkm(radi1), vphase, co=green()
run_wavelet, radi, vfilt, vwave, /noplot
vphase_filt = unwrap_phase(get_phase( total(vwave,2), wrap=vwrap) )
oplot, tkm(radi), vphase_filt, co=red()                                
oplot, tkm(radi), vphase_filt-360, co=red()
oplot, tkm(radi), vphase_filt-720, co=red()
oplot, tkm(radi), poly( radi-rres0, fit1 ), co=green(), l=2
oplot, tkm(radi), poly( radi-rres0, fit1 )-360, co=green(), l=2
print, fit1

end
