; The simulated profiles for the Mimas 5:3 wave as carried out by
; Rappaport et al. (2009, Icarus)
a1 = 132260.0d0
ac = 132275.0d0
an = 132470.0d0
aa = dindgen(an-a1) + a1
zeta = 50.0d0
ae = 0.1d0 + 2*( 1 + tanh((aa-ac)/zeta) )*(aa-an)/(a1-an)
tau0 = 0.5d0 + 0.5*( 1 + tanh((aa-ac)/zeta) )*(aa-an)/(a1-an)
opacity = 0.0125  ;cm^2/g
sigma0 = tau0 / opacity

!p.multi = [0,2,4]
!p.charsize = 2
plot, aa-a1, tau0, /xs, ytit='!Mt!D0!N'
plot, aa-a1, ae, /xs, ytit='ae'
plot, aa-a1, sigma0, /xs, ytit='!Ms!D0!N'

end
