pro p2radec_u,pixscale,cmat,nl,line,sample,RA,dec

if n_params() eq 0 then begin
  print, 'Syntax:  P2RADEC, cam_params, cmat, nl, line, sample, RA, Dec'
  return
endif

p2radec_quicker_u,pixscale,cmat,nl,line,sample,RA,dec

end

