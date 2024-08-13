!p.multi=[0,2,2]
.run c_delta2
!p.multi=[0,2,2]
plot_blank, /noerase, tit='Torrence/Compo method: C!D!Md!N = 0.778'
!p.multi=[3,2,2]
.run c_delta4
!p.multi=[3,2,2]
plot_blank, /noerase, tit='Farge/Tiscareno method: C!D!Md!N = 0.811'
plot, omega/2/!pi, psihat01/omega, /noerase, pos=[.68,.67,.83,.83], $
      /xs, /ys, xr=[0,2]


