cd, '$DATA/images/046/RDHRESSCN'
@sigma_ref_046rdhresscn
print, 'Black is 046'
radius046 = reform(_keywords.ringplane_aimpoint_radius)
sigma046 = radscan_sigma
cd, '$DATA/images/071/RDHRSSCHP'
restore, 'stretch.sav'
restore, 'ring_rads_index.sav.old8'
radius071 = reform(_keywords.ringplane_aimpoint_radius)
sigma071 = radscan_sigma
oplot, tkm(radius071), sigma071, ps=-4, l=1, co=ctblue()
print, 'Blue is 071'
cd, '$DATA/images/077/RDHRCOMP'
restore, 'stretch.sav'
radius077 = reform(_keywords.ringplane_aimpoint_radius)
sigma077_init = interpol( sigma071, radius071, radius077 )
if keyword_set(follow046) then sigma077_init = interpol( sigma046, radius046, radius077 )
oplot, tkm(radius077), sigma077_init, ps=4, co=ctgreen()
if not keyword_set(follow046) then follow046 = 0
print, 'Green is initial values for 077, based on '+(['071','046'])[follow046]
restore, 'ring_rads_index.sav'
sigma077 = radscan_sigma
oplot, tkm(radius077), sigma077, ps=-4, co=ctyellow()
print, 'Yellow is 077'
sigma071_new = interpol( sigma077, radius077, radius071 )
oplot, tkm(radius071), sigma071_new, ps=-4, co=ctcyan()
print, 'Cyan is new 071, now based on 077'
cd, '$DATA/images/132/PROPELLR'
restore, 'stretch.sav'
restore, 'ring_rads_index.sav'
radius132 = reform(_keywords.ringplane_aimpoint_radius)
sigma132 = radscan_sigma
foo = [0,7]
oplot, tkm(radius132[foo]), sigma132[foo], ps=-4, co=ctpurple()
print, 'Purple is 132'
