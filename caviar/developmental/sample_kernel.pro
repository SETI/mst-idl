nn = 1000
sclk = where( label[0,*] eq 'SPACECRAFT_CLOCK_STOP_COUNT', count )
if count eq 1 then sclk=sclk[0] else stop, sclk
sclk = label[1,sclk];76]
_sclk = double(sclk) + (dindgen(nn)-nn/2)*1000
_ra = fltarr(nn)
_dec = fltarr(nn)
for jjj=0,nn-1 do begin
  cspice_scencd,sc,string( _sclk[jjj], fo='(F14.3)' ),sclkdp
  cspice_ckgp,-82000L,sclkdp,1000.0d0,'J2000',pmat,clkout,found
  pointing_ra_dec,nacmat##pmat,__ra,__dec
  _ra[jjj] = __ra
  _dec[jjj] = __dec
endfor

!p.multi = [0,1,2]
xtit = 'Spacecraft Clock (sclk)'
plot, _sclk, _ra, /xs, /ys, xtit=xtit, ytit='Right Ascension (deg)'
oplot, 1475690000.0d + [ 7925, 8332, 8683, 9034, 9641 ], replicate(60,5),ps=4
plot, _sclk, _dec, /xs, /ys, xtit=xtit, ytit='Declination (deg)'
oplot, 1475690000.0d + [ 7925, 8332, 8683, 9034, 9641 ], replicate(60,5),ps=4

end
