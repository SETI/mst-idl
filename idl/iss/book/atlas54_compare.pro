!p.multi = [0,4,4]

imdir = '/home/borogove/iss/images/'
ebdir = '/home/borogove/iss/images/SOI/SOISPTURN/emily/feb06/'
radscan_keepwin = 1

restore, imdir+'SOI/SOISPTURN/N1467345208_2_cal.scan1'
radscan_xr = [ 118.825, 118.852 ]
radscan_tit = 'SOI/SOISPTURN (Tiscareno)'
.run plot_radscan

restore, ebdir+'dark_side_rad_scan_tiscareno.idl.sav'
jjj = 15
if eb_filenumber[jjj] ne '5208' then stop
plot_nosci, tkm((*(eb_dsk_pts[jjj]))[*,0]/1000)+.008, *(eb_profile[jjj]), $
            xr=radscan_xr, /xs, /ys, tit='SOI/SOISPTURN (Baker)', $
            xtit='Radius'+tkmtit(), ytit='I/F'

restore, imdir+'008/RDHRCOMP/N1495327885_1_cal.scan2'
radscan_xr = [ 118.825, 118.852 ]
radscan_tit = '008/RDHRCOMP (Tiscareno)'
.run plot_radscan

restore, ebdir+'rev8_rad_scans.idl.sav'
jjj = 11
if eb_filenumber[jjj] ne '27885' then stop
plot_nosci, tkm((*(eb_dsk_pts[jjj]))[*,0]/1000), *(eb_profile[jjj]), $
            xr=radscan_xr, /xs, /ys, tit='008/RDHRCOMP (Baker)', $
            xtit='Radius'+tkmtit(), ytit='I/F'

restore, imdir+'026/RDHRESSCN/N1532372939_1_cal.scan1'
radscan_xr = [ 118.825, 118.852 ]
radscan_tit = '026/RDHRESSCN'
.run plot_radscan

restore, imdir+'028/RDHRESSCN/N1536499158_1_cal.scan1'
radscan_xr = [ 118.825, 118.852 ]
radscan_tit = '028/RDHRESSCN'
.run plot_radscan

restore, imdir+'031/RDHRCOMP/N1540679027_1_cal.scan1'
radscan_xr = [ 118.825, 118.852 ]
radscan_tit = '031/RDHRCOMP'
.run plot_radscan

restore, imdir+'032/RDHRCOMP/N1541714632_1_cal.scan1'
radscan_xr = [ 118.825, 118.852 ]
radscan_tit = '032/RDHRCOMP'
.run plot_radscan

!p.multi[0] = !p.multi[0] - 1

restore, imdir+'046/RDHRESSCN/N1560311927_1_cal.scan1'
radscan_xr = [ 118.825, 118.852 ]
radscan_tit = '046/RDHRESSCN'
.run plot_radscan

restore, imdir+'056/RDHRESSCN/N1579167773_1_cal.scan1'
radscan_xr = [ 118.825, 118.852 ]
radscan_tit = '056/RDHRESSCN'
.run plot_radscan

;end
