print, 'Initializing...'
nyear = 10
year = '20'+string(sindgen(nyear)+7,fo='(I02)')
hours = string(sindgen(24),fo='(I02)')
for y=0,nyear-1 do begin
  if year[y]/4.0d0 mod 1 eq 0 then nday = 366 else nday = 365
  _time = strarr(nday*24)
  day = string(sindgen(nday)+1,fo='(I03)')
  for d=0,nday-1 do begin
    _time[d*24:d*24+23] = year[y]+'-'+day[d]+'T'+hours+':00'
  endfor 
  if y eq 0 then time = _time else time = [ time, _time ]
endfor
;time = clip(time)
time = time[24:24*6]
nt = n_elements(time)

cspice_furnsh, getenv("CAVIAR_KERNELS")+'.prop'
cspice_furnsh, '/home/sauron2/iss/NAIF/lsk/naif0010.tls'
cspice_furnsh, '/home/sauron2/iss/NAIF/spk/sat363.bsp'
cspice_furnsh, '/home/sauron2/iss/NAIF/pck/pck00010.tpc'

cspice_str2et, time, et

print, 'Getting positions...'
pos = dblarr(nt,3)
if keyword_set(dovel) then vel = dblarr(nt,3)
for j=0l,nt-1 do begin
  cspice_spkez, 699001L, et[j], 'IAU_SATURN', 'CN+S', 699l, state, ltime
  pos[j,*] = state[0:2]
  if keyword_set(dovel) then vel[j,*] = state[3:5]
endfor

polar = cart_to_polar( pos )

if keyword_set(dolzr) then begin
  lzr, 'bleriot_test_140627'
  @plot_prepare
endif
!p.multi = [0,1,3]
!p.charsize = 2
!y.margin = 0
!y.omargin = [4,2]
notn = replicate(' ',20)
;lonmodel = poly_fit( time/86400/365.25, polar[*,1], 1 )
;ytime = et/86400/365.25+2000
;xtit = 'Year'
ytime = (et-et[0])/86400
xtit = 'Days after '+time[0]
plot, ytime, polar[*,0], /xs, xtickn=notn, ytit='Vertical (km)'
plot, ytime, polar[*,1]*180/!dpi, /xs, xtickn=notn, ytit='Longitude (!Uo!N)', $
      /ys, yr=[-180,180], yticki=90
plot_nosci, ytime, polar[*,2], /xs, /ys, xtit=xtit, ytit='Radial (km)'
if keyword_set(dolzr) then clzr

end
