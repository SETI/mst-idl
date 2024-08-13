; Radial scans generated before 2 May 2005 contain an error in which 
; radscan_np, the array containing the number of pixels in each bin, is 
; too large by exactly a factor of 100.
scanfiles = findfile('*.scan?')
for jjj=0,n_elements(scanfiles)-1 do begin
  restore, scanfiles[jjj]
  radscan_np = radscan_np / 100
  save, errbar, phiout, radi, radscan_cmat, radscan_descrip, radscan_np, val, filename=scanfiles[jjj]
endfor

end

