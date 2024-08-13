pro p2ralon,cmat,et,polera,poledec,sc,RA,dec,radius,lon,$
                    outofplane=outofplane, light_time=light_time, $
            xyz_rp=xyz_rp, n_iter=n_iter

if n_params() eq 0 then begin
  print, 'Syntax:  P2RALON, cmat, et, polera, poledec, sc, RA, Dec, radius, lon'
  return
endif

p2ralon_quicker,cmat,et,polera,poledec,sc,RA,dec,radius,lon,$
                    outofplane=outofplane, light_time=light_time, $
                xyz_rp=xyz_rp, n_iter=n_iter

end

