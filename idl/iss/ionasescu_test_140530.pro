; See emails from Rodica Ionasescu in the second half of May 2014. 
kerndir = '/home/sauron2/iss/NAIF/'
cspice_furnsh, kerndir + 'pck/pck00010.tpc'
cspice_furnsh, kerndir + 'lsk/naif0010.tls'
cspice_furnsh, kerndir + 'spk/BleriotMean.bsp'
date = '2007-100T00:00'
cspice_str2et, date, et
nt = 1500l
pos = dblarr(nt,3)
time = et + dindgen(nt)*60
for j=0l,nt-1 do begin
  cspice_spkez, -6001, time[j], 'J2000', 'NONE', 699, state, ltime
  pos[j,*] = state[0:2]
endfor

polar = cart_to_polar(pos)
wrap = get_phase_wrap( polar[*,1], /radians, wrapsign=wrapsign )
foo = where( wrapsign eq 1, count )
if count lt 2 then stop
if polar[nt-1,1] le 0 then stop
zero1 = interpol( time[wrap[foo[0]]:wrap[foo[1]]-1]-et, $
                  polar[wrap[foo[0]]:wrap[foo[1]]-1,1], 0 )
zero2 = interpol( time[wrap[foo[1]]:wrap[foo[1]+1]-1]-et, $
                  polar[wrap[foo[1]]:wrap[foo[1]+1]-1,1], 0 )

if keyword_set(dolzr) then begin
  lzr, 'ionasescu_test_140530'
  @plot_prepare
endif
!p.multi = [0,2,2]
xtit='Seconds after '+date
plot_nosci, time-et, polar[*,2], /xs, /ys, ytit='Radius (km)', xtit=xtit
plot_nosci, time-et, polar[*,1]*180/!dpi, /xs, /ys, ytit='Longitude (!Uo!N)', yticki=90, yr=[-180,180], xtit=xtit
solid_circles
oplot, [zero1,zero2], [0,0], ps=8
plot_nosci, time-et, polar[*,0], /xs, /ys, ytit='Height (km)', xtit=xtit
if keyword_set(dolzr) then clzr

print, zero1, zero2
print, 360.0d0/(zero2-zero1)*86400
print, caviar_omega_to_r( !dpi*2/(zero2-zero1) ), caviar_omega_to_r( !dpi*2/(zero2-zero1), j2=0, j4=0, j6=0 )

end
