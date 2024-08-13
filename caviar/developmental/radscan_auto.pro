for jjj=0,n_elements(filenames)-1 do begin
  print, jjj
  image_name=filenames[jjj]
  @caviar
  @radscan
  radscan_cmat = cmat
  save, radi, val, errbar, radscan_cmat, radscan_np, phiout, radscan_descrip, $
           filename=filestem+'.scan1'
endfor

end

