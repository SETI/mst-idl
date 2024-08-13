device, decomposed=0
restore, 'c_delta2.sav'
restore, 'c_delta4.sav'
!p.multi=[0,2,2]
if keyword_set(dolzr) then begin
  lzr, 'c_delta1', /half
  @plot_prepare
  plot_color
endif
;plot, _omega0, cd2, /nodata, xtit='!Mw!D0!N', ytit='C!D!Md!N', $
;      tit='C!D!Md!N from TC98 (red) and Tiscareno (blue)'
;oplot, _omega0, cd2, co=red()
;oplot, _omega0, cd4, co=blue()
plot, _omega0, cd2, /nodata, xtit='!Mw!D0!N', ytit='C!D!Md!N', /xlog, /ylog, $
      /xs, /ys, tit='C!D!Md!N from TC98 (red) and Tiscareno (blue)', yr=[.2,1]
oplot, _omega0, cd2, co=red()
oplot, _omega0, cd4, co=blue()
a = svdfit( alog10(_omega0), alog10(cd2), 2, sigma=asigma )
b = svdfit( alog10(_omega0), alog10(cd4), 2, sigma=bsigma )
print, 'C_delta_TC98 = ('+strtrim(10^a[0]*6^a[1],2)+'+-'+$
       strtrim(alog(10)*10^a[0]*asigma[0]*6^a[1],2)+') * (omega0/6) ^ ('+$
       strtrim(a[1],2)+'+-'+strtrim(asigma[1],2)+')'
print, 'C_delta_Tisc = ('+strtrim(10^b[0]*6^b[1],2)+'+-'+$
       strtrim(alog(10)*10^b[0]*bsigma[0]*6^b[1],2)+') * (omega0/6) ^ ('+$
       strtrim(b[1],2)+'+-'+strtrim(bsigma[1],2)+')'
xyouts, 9.8, .4, '            ('+string(10^a[0]*6^a[1],fo='(F6.4)')+pmsym()+$
        string(alog(10)*10^a[0]*asigma[0]*6^a[1],fo='(F6.4)')+$
        ') * (!Mw!D0!N/6)!U'+string(a[1],fo='(F7.4)')+pmsym()+$
        string(asigma[1],fo='(F6.4)')+'!N', align=.5, co=red(), orient=-31.7
xyouts, 10, .5, '            ('+string(10^b[0]*6^b[1],fo='(F6.4)')+pmsym()+$
        string(alog(10)*10^b[0]*bsigma[0]*6^b[1],fo='(F6.4)')+$
        ') * (!Mw!D0!N/6)!U'+string(b[1],fo='(F7.4)')+pmsym()+$
        string(bsigma[1],fo='(F6.4)')+'!N', co=blue(), align=.5, orient=-31.7
;xyouts, 10, .4, 'C!D!Md!N = ' + strtrim(10^a[0],2) + ' * !Mw!D0!N!U' + $
;        strtrim(a[1],2), align=1, co=red()
;xyouts, 10, .5, 'C!D!Md!N = ' + strtrim(10^b[0],2) + ' * !Mw!D0!N!U' + $
;        strtrim(b[1],2), co=blue()
solid_diamonds
oplot, [6], cd2[where(_omega0 eq 6)], ps=8, co=red()
xyouts, [6], cd2[where(_omega0 eq 6)]-.08, co=red(), $
        string(cd2[where(_omega0 eq 6)], fo='(F6.4)'), /align
oplot, [6], cd4[where(_omega0 eq 6)], ps=8, co=blue()
xyouts, [6.2], cd4[where(_omega0 eq 6)]+.05, co=blue(), $
        string(cd4[where(_omega0 eq 6)], fo='(F6.4)')
axis, /data, xaxis=0, xtickv=[5,20], /xticks
axis, /data, yaxis=0, ytickv=[.2,.5], /yticks
plot, _omega0, cd2 - 10^a[0]*_omega0^a[1], /nodata, xtit='!Mw!D0!N', $
      ytit='C!D!Md!N - A!Mw!D0!N!UB!N', tit='Power Law Fit Residuals'
oplot, _omega0, cd2 - 10^a[0]*_omega0^a[1], co=red()
oplot, _omega0, cd4 - 10^b[0]*_omega0^b[1], co=blue()
plot_nosci, _omega0, cd4/cd2, /xs, /ys, yr=[1.04128,1.04135], $
            xtit='!Mw!D0!N', ytit='C!D!Md!N!UTisc!N / C!D!Md!N!UTC98!N', $
            tit='Ratio between Tiscareno and TC98 calculations', xticki=5
radi = findgen(1000)/10
val = sin(radi)
wavelet_noplot=1
@run_wavelet1
!p.charsize = 1
psi0 = !dpi^(-0.25)
cdelta = (cd4[where(_omega0 eq 6)])[0]
vfilt = wavelet_dj * sqrt(wavelet_dt) / (cdelta*psi0) * $
                               ( float(wave)  # (1./sqrt(wavelet_scale)) )
plot, radi, val - vfilt[0:998], /nodata, /ys, l=1, xtit='x', $
      tit='Reconstructed Sine Wave Residual', ytit='sin(x) - Reconstruction'
oplot, radi, val - vfilt, co=blue()
xyouts, 95, .06, 'C!D!Md!N = '+string(cdelta,fo='(F5.3)'), co=blue(), /align
cdelta = (cd2[where(_omega0 eq 6)])[0]
vfilt = wavelet_dj * sqrt(wavelet_dt) / (cdelta*psi0) * $
                               ( float(wave)  # (1./sqrt(wavelet_scale)) )
oplot, radi, val - vfilt, co=red()
xyouts, 95, .05, 'C!D!Md!N = '+string(cdelta,fo='(F5.3)'), co=red(), /align
cdelta = 0.776
vfilt = wavelet_dj * sqrt(wavelet_dt) / (cdelta*psi0) * $
                               ( float(wave)  # (1./sqrt(wavelet_scale)) )
oplot, radi, val - vfilt, co=cyan()
xyouts, 95, .04, 'C!D!Md!N = '+string(cdelta,fo='(F5.3)'), co=cyan(), /align

if keyword_set(dolzr) then clzr

end
