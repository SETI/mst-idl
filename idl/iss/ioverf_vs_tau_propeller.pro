; Values for N1467347210
incidence_angle = 114.47713
emission_angle = 5.3086807

if keyword_set(dolzr) then begin
  lzr, 'ioverf_vs_tau_propeller'
  @plot_prepare
  !p.multi = [0,2,2]
endif

tau1 = 4
ioverf = ioverf_vs_tau( incidence_angle, emission_angle, _tau=tau, tau1=tau1, $
                        /notaumax, taumax=taumax )
ntau = n_elements(tau) - 1

pts = [ .2, .3, .4, 3.5 ]
solid_circles
oplot, pts, ioverf(pts*ntau/tau1), ps=8
xyouts, pts[0:2], ioverf(pts[0:2]*ntau/tau1), $
        '!C'+[ '!Mt!Dprop!N?', '!Mt!Dgap!N', '!Mt!Dprop!N?' ]
xyouts, pts[3], ioverf(pts[3]*ntau/tau1)+.03, '!Mt!Dwake!N'

if keyword_set(dolzr) then clzr

end
