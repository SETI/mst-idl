;if keyword_set(object_offset) then begin
if not keyword_set(object_offset) then object_offset = [ 0., 0. ] &$

  cmat_orig = cmat &$
  cspice_m2eul,cmat_orig,3,1,3,ang3,ang2,ang1 &$
  twiststart=ang3 &$
  decstart=((0.5d0*!dpi)-ang2) &$
  rastart=(ang1-(0.5d0*!dpi)) &$
  if rastart lt 0.0d0 then rastart=rastart+(2.0d0*!dpi) &$
  if twiststart lt 0.0d0 then twiststart=twiststart+(2.0d0*!dpi) &$

  twist=twiststart+(!dpi/2.0d0) &$

  rot=[[cos(-twist),sin(-twist),0.0d0],[-sin(-twist),cos(-twist),0.0d0],[0.0,0.0,1.0d0]] &$

  start=[-object_offset[1]*nl*2,object_offset[0]*nl*2,20.0d0] &$
  start=start*1.0d0 &$

  stop=dblarr(3) &$
  d_stop=rot##start &$
  stop[0:2]=d_stop[0,0:2] &$

  dec=decstart+stop[1]*cam_params[11] &$

  ra=rastart+(stop[0]*cam_params[11]/cos(dec)) &$

  cspice_eul2m,twiststart,((0.5d0*!dpi)-dec),((0.5d0*!dpi)+ra),3,1,3,cmat

;endif

;end
