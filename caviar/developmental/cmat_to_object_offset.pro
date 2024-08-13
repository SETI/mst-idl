; To define an object_offset, the original cmat *must* be entered as cmat_orig

cspice_m2eul,cmat,3,1,3,ang3,ang2,ang1
dec=((0.5d0*!dpi)-ang2)
ra=(ang1-(0.5d0*!dpi))
if ra lt 0.0d0 then ra=ra+(2.0d0*!dpi)

cspice_m2eul,cmat_orig,3,1,3,ang3,ang2,ang1
twiststart=ang3
decstart=((0.5d0*!dpi)-ang2)
rastart=(ang1-(0.5d0*!dpi))
if rastart lt 0.0d0 then rastart=rastart+(2.0d0*!dpi)
if twiststart lt 0.0d0 then twiststart=twiststart+(2.0d0*!dpi)

twist=twiststart+(!dpi/2.0d0)

rot=[[cos(-twist),sin(-twist),0.0d0],[-sin(-twist),cos(-twist),0.0d0],[0.0,0.0,1.0d0]]

object_rot = 1
object_rot_angle = 0.
object_rot_axis = [ 0.5, 0.5 ]

stop = dblarr(3)
stop = [ (ra-rastart)*cos(dec)/cam_params[11], (dec-decstart)/cam_params[11], 20 ]
d_stop = rotate( stop, 1 )
start = invert(rot) ## d_stop
object_offset = float( [ start[1], -start[0] ] / nl / 2 )

end