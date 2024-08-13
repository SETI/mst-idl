;!p.multi=[0,2,2]
;@plot_prepare
; Find the location of Janus 2:1
if not keyword_set(rl1) or not keyword_set(rl2) then begin
  rl1 = 2
  rl2 = 1
endif
if rl1 eq 2 and rl2 eq 1 then j21 = 1 else j21 = 0
rres = resloc(rl1,rl2,610,res_descrip=res_descrip)
; Define an array of surface densities from 30 to 160 g/cm^2
if keyword_set(j21) then sigma = findgen(130)+30 else sigma = findgen(40)+10

; Shu's chapter, Eq.41, says c_g = pi * G * sigma / kappa
kappa = sqrt(caviar_kappa2(rres))
groupv = !dpi * 6.672e-8 * sigma / kappa * 3.16e7/1e5
plot, sigma, groupv, /xs, ys=9, xtit='Surface Density (g/cm^2)', $
      ytit='Group Velocity (km/yr)', xma=[10,10], $
      tit='At '+res_descrip+', kappa='+string(kappa,fo='(F8.6)')+' s^-1'
axis, yaxis=1, yr=[min(groupv),max(groupv)]*1460/365.25, /ys, $
      ytit='Distance after 1460 days (km)'
;axis, yaxis=1, yr=[min(groupv),max(groupv)]*880/365.25, /ys, $
;      ytit='Distance after 880 days (km)'

end
