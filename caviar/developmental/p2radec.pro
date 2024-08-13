pro p2radec,cam_params,cmat,nl,line,sample,RA,dec

if n_params() eq 0 then begin
  print, 'Syntax:  P2RADEC, cam_params, cmat, nl, line, sample, RA, Dec'
  return
endif

p2radec_quicker,cam_params,cmat,nl,line,sample,RA,dec

end

