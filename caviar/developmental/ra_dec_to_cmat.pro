function ra_dec_to_cmat, ra, dec, twist

  if n_params() eq 0 then begin
    print, 'Syntax:  cmat = ra_dec_to_cmat( ra, dec, twist )'
    return, -1
  endif

  cspice_eul2m,twist,((0.5d0*!dpi)-dec),((0.5d0*!dpi)+ra),3,1,3,cmat
  return, cmat

end
