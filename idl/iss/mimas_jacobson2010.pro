; Mimas mean longitude from a memo received from Bob Jacobson on 16 July 2010

l0 = 160.384d0
n0 = 381.99449342d0
aa = [ 0.086d0, 43.394d0, 0.038d0, 0.040d0, $
       0.711d0, 0.006d0, 0.018d0, 0.006d0, $
       0.007d0, 0.007d0, 0.005d0, 0.012d0, $
       0.062d0, 0.132d0, 0.058d0, 0.006d0, $
       0.0070 ]
s0x_l = [ 140.522194d0, 140.783607d0, 327.102564d0, 280.302958d0, $
          62.373816d0, 300.040971d0, 344.546852d0, 191.721137d0, $
          303.153089d0, 47.584802d0, 343.715771d0, 164.842791d0, $
          326.715310d0, 289.088058d0, 68.287924d0, 245.673038d0, $
          104.289884d0 ] * !dpi/180
s0x_n = [ 360.0049282d0, 509.6912597d0, 720.0098564d0, 1034.6209028d0, $
          1528.6344963d0, 2160.0295692d0, 2506.9383875d0, 2880.0394256d0, $
          28763.7730191d0, 29768.0499596d0, 56880.7786554d0, 57600.7885118d0, $
          57918.0956381d0, 58425.8893310d0, 58931.9525514d0, 59040.8082246d0, $
          59760.8180819d0 ] * !dpi/180
num = n_elements(aa)
if n_elements(s0x_l) ne num then stop
if n_elements(s0x_n) ne num then stop

if not keyword_set(d0) then d0 = julday(1,1,1900)
if not keyword_set(d1) then d1 = julday(1,1,2100)
j2000 = julday(1,1,2000)
if not keyword_set(npd) then npd = 0.1  ; Number per day
dd = dindgen((d1-d0)*npd+1)/npd + d0 - j2000  ; Time in days from J2000
tt = dd / 36525  ; Time in Julian centuries from J2000
yy = dd / 365.25 + 2000

dtheta = tt*0
for j=0,num-1 do begin
  dtheta = dtheta + aa[j]*sin( s0x_l[j] + s0x_n[j]*tt )
endfor
theta = l0 + n0*dd + dtheta

dn = tt*0
for j=0,num-1 do begin
  dn = dn + aa[j]*s0x_n[j]/36525*cos( s0x_l[j] + s0x_n[j]*tt )
endfor
nn = n0 + dn

plot_nosci, yy, caviar_omega_to_r( nn*!dpi/180/86400 ), /xs, /ys, $
            xtit='Year', ytit='Semimajor Axis (km)'

end

