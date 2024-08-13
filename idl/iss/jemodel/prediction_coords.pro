; Find where the spacecraft is looking at prediction times.  

iterate = 1
usedate = 1
ii = 0

; 028/RDHRESSCN001, Begin 2006-252T08:30:00, Duration 1:30
tit = '028/RDHRESSCN001'
psname = '028_rdhresscn001'
date = '2006-252T08:30:00'
dt = 30
nt = 90 * 60. / dt
xxr = [8,82.5] * 60. / dt

;; 028/RDHRESSCN002, Begin 2006-252T12:15:00, Duration 1:58
;tit = '028/RDHRESSCN002'
;psname = '028_rdhresscn002'
;date = '2006-252T12:15:00'
;dt = 30
;nt = 118 * 60. / dt
;xxr = [10,108] * 60. / dt

;; 031/RDHRCOMP, Begin 2006-300T20:00:00, Duration 3:00
;tit = '031/RDHRCOMP'
;psname = '031_rdhrcomp'
;date = '2006-300T20:00:00'
;dt = 30
;nt = 180 * 60. / dt
;xxr = [5,173] * 60. / dt
;cspice_furnsh, '/home/borogove/iss/NAIF/ck/ISS_031RI_RDHRCOMP001_PRIME_4.bc'

;; 032/RDHRCOMP, Begin 2006-312T19:45:00, Duration 2:55
;tit = '032/RDHRCOMP'
;psname = '032_rdhrcomp'
;date = '2006-312T19:45:00'
;dt = 30
;nt = 175 * 60. / dt
;xxr = [7,162] * 60. / dt
;cspice_furnsh, '/home/borogove/iss/NAIF/ck/ISS_032RI_RDHRCOMP001_PRIME_3.bc'

;; 046/RDHRESSCN, Begin 2007-163T02:30:00, Duration 2:00
;tit = '046/RDHRESSCN'
;psname = '046_rdhresscn'
;date = '2007-163T02:30:00'
;dt = 30
;nt = 120 * 60. / dt
;xxr = [7,111] * 60. / dt
;cspice_furnsh, '/home/borogove/iss/NAIF/ck/ISS_046RI_RDHRESSCN001_PRIME_2.bc'

;; 056/RDHRESSCN, Begin 2008-016T08:30:30, Duration 2:00
;tit = '056/RDHRESSCN'
;psname = '056_rdhresscn'
;date = '2008-016T08:35:00'
;dt = 30
;nt = 120 * 60. / dt
;xxr = [0,107.5] * 60. / dt
;cspice_furnsh, '/home/borogove/iss/NAIF/ck/ISS_056RI_RDHRESSCN001_PRIME_3.bc'

if keyword_set(psname+'.sav') then restore, psname+'.sav'

.run kernel_access

t75 = interpol( (indgen(nk)*dt/60)[int], $
                _keywords[int].ringplane_aimpoint_radius, resloc(7,5,610) )
l75 = interpol( _keywords.ringplane_aimpoint_longitude, $
                indgen(nk)*dt/60, t75 )   
rs75 = interpol( _keywords.ringplane_aimpoint_radial_scale, $
                 indgen(nk)*dt/60, t75 )   
t97 = interpol( (indgen(nk)*dt/60)[int], $
                _keywords[int].ringplane_aimpoint_radius, resloc(9,7,610) )
l97 = interpol( _keywords.ringplane_aimpoint_longitude, $
                indgen(nk)*dt/60, t97 )   
rs97 = interpol( _keywords.ringplane_aimpoint_radial_scale, $
                 indgen(nk)*dt/60, t97 )   
t119 = interpol( (indgen(nk)*dt/60)[int], $
                 _keywords[int].ringplane_aimpoint_radius, resloc(11,9,610) )
l119 = interpol( _keywords.ringplane_aimpoint_longitude, $
                 indgen(nk)*dt/60, t119 )   
rs119 = interpol( _keywords.ringplane_aimpoint_radial_scale, $
                  indgen(nk)*dt/60, t119 )   

save, date, t75, t97, t119, l75, l97, l119, rs75, rs97, rs119, $
      filename=psname+'.sav'
