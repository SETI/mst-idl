if keyword_set(plot_vgr) then begin &$

  filenum = fix(( tkmrev( radscan_xr, thoukm=thoukm ) - (tkmrev( radscan_xr, thoukm=thoukm ) mod 10000) )/1000) &$
  vgrfile = strtrim(filenum[0],2) &$
  if strlen(vgrfile) eq 2 then vgrfile = '0'+vgrfile &$
  restore,'~/Data/images/Voyager/VGR_PPS/KM001/vgr_pps_'+vgrfile+'.sav' &$
  vgr_pps_radi = *data.radius &$
  vgr_pps_tau = *data.tau &$
  restore,'~/Data/images/Voyager/VGR_RSS/KM001/vgr_rss_'+vgrfile+'.sav' &$
  vgr_rss_radi = *data.radius &$
  vgr_rss_tau = *data.tau &$
  if filenum[1] ne filenum[0] then begin &$
    vgrfile = strtrim(filenum[1],2) &$
    if strlen(vgrfile) eq 2 then vgrfile = '0'+vgrfile &$
    restore,'~/Data/images/Voyager/VGR_PPS/KM001/vgr_pps_'+vgrfile+'.sav' &$
    vgr_pps_radi = [ vgr_pps_radi, *data.radius ] &$
    vgr_pps_tau = [ vgr_pps_tau, *data.tau ] &$
    restore,'~/Data/images/Voyager/VGR_RSS/KM001/vgr_rss_'+vgrfile+'.sav' &$
    vgr_rss_radi = [ vgr_rss_radi, *data.radius ] &$
    vgr_rss_tau = [ vgr_rss_tau, *data.tau ] &$
  endif &$
  j0 = tkmrev( radscan_xr[0], thoukm=thoukm ) - filenum[0]*1000 &$
  j1 = tkmrev( radscan_xr[1], thoukm=thoukm ) - filenum[0]*1000 &$

endif

;end
