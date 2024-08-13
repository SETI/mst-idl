function caviar_nu2, r, domega_dr, domega2_dr=domega2_dr, lc82=lc82, gm=_gm, prad=_prad, j2=_j2, j4=_j4, j6=_j6, j8=_j8, noj8=noj8

nu2 = caviar_omega2( r, domega_dr, domega2_dr=domega2_dr, lc82=lc82, gm=_gm, prad=_prad, j2=_j2, j4=_j4, j6=_j6, j8=_j8, noj8=noj8, var='nu' )

return, nu2

end
