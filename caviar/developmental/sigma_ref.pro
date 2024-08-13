savefile = '$DATA/caviar/developmental/sigma_ref.sav'
if keyword_set(findfile(savefile)) then restore, savefile else begin

  restore, '$DATA/images/SOI/SOISPTURN/emily/feb06/sigmanu_data.sav'
  restore, '$DATA/images/116/EQXSHADOW001/iawave_sigmafit.sav'

  ; Generalize variables for iawave
  iawave_radi = radi
  iawave_sigma = sigmafit
  iawave_rrname = 'Ia -1:0 BW'
  iawave_source = 'Tiscareno et al. (2013, Icarus)'

  ; Add data from Baillie et al. (2011, Icarus), Table 6
  baillie_rrname = [ 'Mi 4:1', 'Ti -1:0 BW', 'At 2:1', 'Mi 6:2', 'Pd 4:2' ]
  ;baillie_rres = [ 74891.8, 77511.3, 87646.5, 89883.3, 89894.0 ]
  baillie_rres = [ resloc(4,1,601,mm=m1), resloc(-1,0,606,/bend,mm=m2), $
                   resloc(2,1,615,mm=m3), resloc(6,2,601,mm=m4), $
                   resloc(4,2,617,mm=m5) ]
  baillie_sigma = [ 0.58, 0.60, 0.22, 1.31, 1.42 ]
  baillie_xid = [ 4.23, 5.14, 5.42, 6.61, 6.69 ]
  baillie_scripty_d = 3.0d0 * ([m1,m2,m3,m4,m5]-1) * caviar_omega2(baillie_rres)
  cap_g = 6.672e-8  ; cm^3 / g / s^2
  baillie_viscosity = 9.0d0 / ( 7 * sqrt(caviar_kappa2(baillie_rres)) * $
                                baillie_xid^3 ) * $
                      sqrt( baillie_rres * 1e5 / baillie_scripty_d * $
                            (2*!dpi*cap_g*baillie_sigma)^3 )
  cassini_rres = [ cassini_rres, baillie_rres ]
  cassini_rrname = [ cassini_rrname, baillie_rrname ]
  cassini_sigma = [ cassini_sigma, baillie_sigma ]
  cassini_goodsig = [ cassini_goodsig, replicate(1,5) ]
  cassini_viscosity = [ cassini_viscosity, baillie_viscosity ]
  cassini_goodvis = [ cassini_goodvis, replicate(1,5) ]
  cassini_source = [ cassini_source, $
                     replicate('Baillie et al. (2011, Icarus)', 5 ) ]
  ncass = ncass + 5

  ; Add data from Tiscareno et al. (2013, Icarus), Footnote 10
  cassini_rres = [ cassini_rres, resloc(5,4,617), resloc(6,5,616) ]
  cassini_rrname = [ cassini_rrname, 'Pd 5:4', 'Pr 6:5' ]
  cassini_sigma = [ cassini_sigma, 28, 38 ]
  cassini_goodsig = [ cassini_goodsig, replicate(1,2) ]
  cassini_viscosity = [ cassini_viscosity, -1, -1 ]
  cassini_goodvis = [ cassini_goodvis, replicate(0,2) ]
  cassini_source = [ cassini_source, $
                     replicate('Tiscareno et al. (2013, Icarus)', 2 ) ]
  ncass = ncass + 2

  ; Add variable cassini_nonlinear to exclude non-linear waves
  space = strpos( cassini_rrname, ' ' )
  colon = strpos( cassini_rrname, ':' )
  bw = strpos( cassini_rrname, 'BW' )
  bw[where( bw ne -1 )] = 1
  bw[where( bw eq -1 )] = 0
  moon = strarr(ncass)
  rl1 = strarr(ncass)
  rl2 = strarr(ncass)
  for j=0,ncass-1 do moon[j] = strmid( cassini_rrname[j], 0, space[j] )
  for j=0,ncass-1 do rl1[j] = strmid( cassini_rrname[j], $
                                      space[j]+1, colon[j]-space[j]-1 )
  for j=0,ncass-1 do rl2[j] = strmid( cassini_rrname[j], colon[j]+1, $
                                      strlen(cassini_rrname[j]) - colon[j]-1 - $
                                      bw[j]*3 )
  order = fix(rl1) - fix(rl2)
  nonlinear = bytarr(ncass)
  nonlinear[where( moon eq 'Ja/Ep' and order eq 1 )] = 1
  nonlinear[where( moon eq 'Pr' and order eq 1 )] = 1
  nonlinear[where( moon eq 'Pd' and order eq 1 )] = 1

  ; Define profiles, the first from Tiscareno et al. (2007, Icarus), Figure 21
  aprof_r = [125,131.8,137]
  aprof_s = [ 33.7 + 1.3*(aprof_r[0:1]-124), 18 ]
  aprof_r = aprof_r * 1000

  save, cassini_rres, cassini_rrname, cassini_sigma, cassini_goodsig, $
        cassini_viscosity, cassini_goodvis, cassini_source, ncass, $
        voyager_rres, voyager_rrname, voyager_sigma, voyager_goodsig, $
        voyager_viscosity, voyager_goodvis, voyager_source, nvoy, $
        iawave_radi, iawave_sigma, iawave_rrname, iawave_source, $
        nonlinear, aprof_r, aprof_s, filename=savefile

endelse

; Make plot of Cassini Division and A Ring
if keyword_set(blankplot) then ymax = 45 else ymax = 60
plot, [117.5,138], [0,ymax], /nodata, /xs, /ys, xtit='Radius'+tkmtit(), $
      ytit='Surface Density (g/cm!U2!N)'
if not keyword_set(blankplot) then begin
  if keyword_set(dps13) then open_circles else solid_circles, radius=0.5
  oplot, tkm(cassini_rres[where(nonlinear)]), $
         cassini_sigma[where(nonlinear)], ps=8
  if keyword_set(dps13) then open_circles else solid_circles
  oplot, tkm(cassini_rres[where(nonlinear eq 0)]), $
         cassini_sigma[where(nonlinear eq 0)], ps=8
  if keyword_set(dps13) then open_circles, radius=0.4 else solid_tiny_circles
  oplot, tkm(voyager_rres), voyager_sigma, ps=8
  if not keyword_set(dps13) then oplot, tkm(aprof_r), aprof_s, l=2
endif
oplot, tkm(iawave_radi), iawave_sigma
oplot, [122.05,122.05], !y.crange, l=1

end
