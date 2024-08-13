if keyword_set(plot_vgr) then begin &$

  ll = [ 'solid', 'dotted' ] &$
  if not keyword_set(vgr_lswitch) then vgr_lswitch = 0 &$
  j0 = tkmrev( radscan_xr[0], thoukm=thoukm ) - float(filenum[0])*1000 &$
  j1 = tkmrev( radscan_xr[1], thoukm=thoukm ) - float(filenum[0])*1000 &$
  plot, tkm( vgr_rss_radi[j0:j1], thoukm=thoukm ), vgr_rss_tau[j0:j1], /xs, $
        ys=9, ytit='Voyager RSS Tau ('+ll[vgr_lswitch]+')', l=vgr_lswitch, $
	xtit = 'Radius'+tkmtit( vgr_rss_radi[j0:j1], thoukm=thoukm ) &$
  axis, yaxis=1, /save, /ys, ytit='Voyager PPS Tau ('+ll[1-vgr_lswitch]+')', $
	yr=[ min(vgr_pps_tau[j0:j1]), max(vgr_pps_tau[j0:j1])<3 ] &$
  oplot, tkm( vgr_pps_radi[j0:j1], thoukm=thoukm ), vgr_pps_tau[j0:j1], $
        l=1-vgr_lswitch &$

endif

;end
