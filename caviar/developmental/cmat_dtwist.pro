  cmat_orig1 = cmat
  cspice_m2eul,cmat_orig1,3,1,3,ang3,ang2,ang1
  twiststart=ang3
  decstart=((0.5d0*!dpi)-ang2)
  rastart=(ang1-(0.5d0*!dpi))
  if rastart lt 0.0d0 then rastart=rastart+(2.0d0*!dpi)
  if twiststart lt 0.0d0 then twiststart=twiststart+(2.0d0*!dpi)

  twiststart=twiststart+_dtwist

  dec=decstart

  ra=rastart

  cspice_eul2m,twiststart,((0.5d0*!dpi)-dec),((0.5d0*!dpi)+ra),3,1,3,cmat
