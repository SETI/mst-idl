restore, '$HOME/idl/iss/impacts/mie/mie_phase_functions.sav' 

if keyword_set(dolzr) then paperplot = 1
if keyword_set(paperplot) then allclouds = 0
if keyword_set(paperplot) then twocx = 1
plot1:
atau = 0.6533
smin = 3e-6
smax = .03
restore, 'impact_table.sav'
foo = indgen(nimpact)
if keyword_set(twocx) then begin
  foo = vec_remove( foo, [ (where( match eq 'Cx ' ))[2], $
                           (where( match eq 'Ax(1)' ))[0], $
                           (where( match eq 'Ax(2)' )), $
                           (where( match eq 'C6' ))[0] ] )
  _theta[(where( match eq 'Cx ' ))[1]] = $
                              mean(_theta[(where( match eq 'Cx ' ))[1:2]])
endif else begin
  foo = vec_remove( foo, [ (where( match eq 'Cx ' ))[1:2], $
                           (where( match eq 'Ax(1)' ))[0], $
                           (where( match eq 'Ax(2)' )), $
                           (where( match eq 'C6' ))[0] ] )
endelse
nfoo = n_elements(foo)

omega = sqrt(caviar_omega2( _radlon[0,*] ))*3600  ; in hours
age = 2./3 / omega / tan(_theta)
if keyword_set(allclouds) then begin
  j = indgen(nfoo)
  nn = floor( age[foo]*omega[foo]/2/!pi * 2 )
  if allclouds eq 2 then qq = 4 else qq = 3
  nnn = n_elements(nn)
  case qq of
    2: m_over_ea_rho = 4.0*(3-2)/3/(4-2)*smax
    3: m_over_ea_rho = 4.0/3/alog(smax/smin)*smax
    4: m_over_ea_rho = 4.0/3*alog(smax/smin)*smin
    5: m_over_ea_rho = 4.0*(5-3)/3/(5-4)*smin
  endcase
  phasei = intarr(n_elements(j))
  for k=0,nfoo-1 do case match[foo[k]] of
    'C1':  phasei[k] = 0
    'C2':  phasei[k] = 0
    'C3':  phasei[k] = 0
    'C4':  phasei[k] = 0
    'C5':  phasei[k] = 0
    'Bx ':  phasei[k] = 1
    'Cx ':  phasei[k] = 1
    'Ax(1)':  phasei[k] = 2
    'Ax(2)':  phasei[k] = 3
    'C6':  phasei[k] = 0
  endcase 
endif else begin
  j = (where( match[foo] eq 'Ax(1)' ))[0]
  ;nn = [ 3, 3, 3, 3, 6, 6, 6, 6 ]
  ;qq = [ 2, 3, 4, 5, 2, 3, 4, 5 ]
  nn = [ 3, 3, 3, 3 ]
  qq = [ 2, 3, 4, 5 ]
  nn = [ nn, nn, nn, nn, nn, nn ]
  qq = [ qq, qq, qq, qq, qq, qq ]
  smin = [ 3e-6, 3e-6, 3e-6, 3e-6, 3e-6, 3e-6, 3e-6, 3e-6, $
           3e-6, 3e-6, 3e-6, 3e-6, 3e-6, 3e-6, 3e-6, 3e-6, $
           3e-5, 3e-5, 3e-5, 3e-5, 3e-7, 3e-7, 3e-7, 3e-7 ]
  smax = [ .03, .03, .03, .03, .003, .003, .003, .003, .3, .3, .3, .3, $
           3, 3, 3, 3, .03, .03, .03, .03, .03, .03, .03, .03 ]
  nnn = n_elements(nn)
  m_over_ea_rho = fltarr(nnn)
  ng = indgen(nnn/4)
  for k=min(qq),max(qq) do begin
    foo1 = where( qq eq k )
    case k of
      2: m_over_ea_rho[foo1] = (4.0*(3-2)/3/(4-2)*smax)[foo1]
      3: m_over_ea_rho[foo1] = (4.0/3/alog(smax/smin)*smax)[foo1]
      4: m_over_ea_rho[foo1] = (4.0/3*alog(smax/smin)*smin)[foo1]
      5: m_over_ea_rho[foo1] = (4.0*(5-3)/3/(5-4)*smin)[foo1]
    endcase
  endfor
  phasei = 2
endelse
if keyword_set(tex) then begin
  f12 = strjoin(replicate('," & ",F12.3',nnn))
  i12 = strjoin(replicate('," & ",I12',nnn))
  e12 = strjoin(replicate('," & ",E12.1',nnn))
endif else begin
  f12 = strjoin(replicate(',F12.3',nnn))
  i12 = strjoin(replicate(',I12',nnn))
  e12 = strjoin(replicate(',E12.1',nnn))
endelse
pp = 2
eatau = _eatau * _mu
eatau = eatau[foo[j]] * phasefac[phasei,pp]/phasefac[phasei,qq-min(qindex)]
eatau_init = eatau*exp(nn*atau)
rho = 1e3
mcloud = eatau_init*1e6 * $     ; m^2
         m_over_ea_rho * $      ; m
         rho                    ; kg/m^3
mcloud = reform(mcloud)
yy = 1e-4
mimp = mcloud * yy
rimp = ( mimp * 3./4/!pi/rho )^(1./3)
if keyword_set(allclouds) then begin
  sigma1 = fltarr(nnn)
  for k=0,nfoo-1 do case strmid(match[foo[k]],0,1) of
    'A':  sigma1[k] = 1000
    'B':  sigma1[k] = 3000
    'C':  sigma1[k] = 10
  endcase 
  sigma2 = sigma1
  sigma2[where( strmid(match[foo],0,1) eq 'A' )] = 400
  rstream = fltarr(nnn)
  for k=0,nfoo-1 do case match[foo[k]] of
    'C1':  rstream[k] = 2*1e3
    'C2':  rstream[k] = 5*1e3
    'C3':  rstream[k] = 3*1e3
    'C4':  rstream[k] = 1*1e3
    'C5':  rstream[k] = 3*1e3
    'Bx ':  rstream[k] = 10*1e3
    'Cx ':  rstream[k] = 2*1e3
    'Ax(1)':  rstream[k] = 50*1e3
    'C6':  rstream[k] = 5*1e3
  endcase 
endif else begin
  sigma1 = fltarr(nnn)
  sigma1[where( rimp gt 120 )] = 400
  sigma1[where( rimp le 120 )] = 1000
  sigma2 = replicate( 400., nnn )
  rstream = replicate( 50*1e3, nnn )
endelse
mtarg1 = !pi*rimp^2*sigma1
mtarg2 = !pi*rstream^2*sigma2
if keyword_set(tex) then begin
  if keyword_set(allclouds) then begin
    print, strjoin(replicate(' ',40))+strjoin('          '+match[foo])
    print, '\hline'
    print, fo='("Number of half-orbits since impact ($N$): "'+i12+'," \\")', nn
    print, fo='("Equivalent area$^a$, km$^2$ ($EA$): "'+i12+'," \\")', $
           round( eatau / 10.^(floor(alog10(eatau))-3) ) * $
           10.^(floor(alog10(eatau))-3)
    print, fo='("Initial equivalent area, km$^2$ ($EA_\mathrm{initial}$): "'+$
           i12+'," \\")', $
           round( eatau_init / 10.^(floor(alog10(eatau_init))-2) ) * $
           10.^(floor(alog10(eatau_init))-2)
    print, fo='("Initial cloud mass, $10^6$ kg ($M_\mathrm{cloud}$): "'+$
           i12+'," \\")', $
           round( mcloud/1e6 / 10.^(floor(alog10(mcloud))-8) ) * $
           10.^(floor(alog10(mcloud))-8)
    print, fo='("Impactor mass, $10^3$ kg ($M_\mathrm{impactor}$): "'+$
           i12+'," \\")', $
           round( round( mimp/1e3 / 10.^(floor(alog10(mimp))-5) ) * $
                  10.^(floor(alog10(mimp))-5) )
    print, fo='("Impactor radius, m ($R_\mathrm{solid~impactor}$): "'+$
           i12+'," \\")', $
           round( round( rimp / 10.^(floor(alog10(rimp))-1) ) * $
                  10.^(floor(alog10(rimp))-1) )
    print, fo='("Target mass available, $10^3$ kg '+$
           '($M_\mathrm{target}^\mathrm{solid~impactor}$): "'+i12+'," \\")', $
           round( round( mtarg1/1e3 / 10.^(floor(alog10(mtarg1))-5) ) * $
                  10.^(floor(alog10(mtarg1))-5) )
    print, fo='("Impactor stream radius, km ($R_\mathrm{impactor~stream}$): "'+$
           i12+'," \\")', round(rstream/1e3)
    print, fo='("Target mass available, $10^6$ kg '+$
           '($M_\mathrm{target}^\mathrm{impactor~stream}$): "'+i12+'," \\")', $
           round( mtarg2/1e6 / 10.^(floor(alog10(mtarg2))-6) ) * $
           10.^(floor(alog10(mtarg2))-6)
  endif else begin
    print, fo='("Number of half-orbits since impact ($N$): "'+i12+'," \\")', nn
    print, fo='("Size-distribution power-law index ($q$): "'+i12+'," \\")', qq
    print, '\hline'
    print, fo='("Equivalent area, km$^2$ ($EA$): "'+i12+'," \\")', $
           round( eatau / 10.^(floor(alog10(eatau))-3) ) * $
           10.^(floor(alog10(eatau))-3)
    print, fo='("Initial equivalent area, km$^2$ ($EA_\mathrm{initial}$): "'+$
           i12+'," \\")', $
           round( eatau_init / 10.^(floor(alog10(eatau_init))-2) ) * $
           10.^(floor(alog10(eatau_init))-2)
    print, fo='("Initial cloud mass, $10^6$ kg ($M_\mathrm{cloud}$): "'+$
           i12+'," \\")', $
           round( mcloud/1e6 / 10.^(floor(alog10(mcloud))-8) ) * $
           10.^(floor(alog10(mcloud))-8)
    print, fo='("Impactor mass, $10^3$ kg ($M_\mathrm{impactor}$): "'+$
           i12+'," \\")', $
           round( round( mimp/1e3 / 10.^(floor(alog10(mimp))-5) ) * $
                  10.^(floor(alog10(mimp))-5) )
    print, fo='("Impactor radius, m ($R_\mathrm{solid~impactor}$): "'+$
           i12+'," \\")', $
           round( round( rimp / 10.^(floor(alog10(rimp))-1) ) * $
                  10.^(floor(alog10(rimp))-1) )
    print, fo='("Target mass available, $10^3$ kg '+$
           '($M_\mathrm{target}^\mathrm{solid~impactor}$): "'+i12+'," \\")', $
           round( round( mtarg1/1e3 / 10.^(floor(alog10(mtarg1))-5) ) * $
                  10.^(floor(alog10(mtarg1))-5) )
    print, fo='("Impactor stream radius, km ($R_\mathrm{impactor~stream}$): "'+$
           i12+'," \\")', round(rstream/1e3)
    print, fo='("Target mass available, $10^6$ kg '+$
           '($M_\mathrm{target}^\mathrm{impactor~stream}$): "'+i12+'," \\")', $
           round( mtarg2/1e6 / 10.^(floor(alog10(mtarg2))-6) ) * $
           10.^(floor(alog10(mtarg2))-6)
  endelse
endif else if keyword_set(allclouds) then begin
  print, strjoin(replicate(' ',41))+strjoin('          '+match[foo])
  print, strjoin(replicate('-',41+12*nfoo))
  print, fo='("Number of half-orbits since impact (N):  "'+i12+')', nn
  print, fo='("Equivalent area, km^2 (tau):             "'+f12+')', eatau
  print, fo='("Initial equivalent area, km^2 (tau):     "'+f12+')', eatau_init
  print, fo='("Initial cloud mass, 10^3 kg (Mcloud):    "'+f12+')', mcloud/1e3
  print, fo='("Impactor mass, kg (Mimp):                "'+f12+')', mimp
  print, fo='("Impactor radius, m (Rimp):               "'+f12+')', rimp
  print, fo='("Target mass available, kg (Mtarg1):      "'+f12+')', mtarg1
  print, fo='("Impactor stream radius, km (Rstream):    "'+f12+')', rstream/1e3
  print, fo='("Target mass available, 10^9 kg (Mtarg2): "'+f12+')', mtarg2/1e9
endif else begin
  print, fo='("Number of half-orbits since impact (N):  "'+i12+')', nn
  print, fo='("Size-distribution power-law index (q):   "'+i12+')', qq
  print, fo='("Minimum size cut-off, m (smin):          "'+e12+')', smin
  print, fo='("Maximum size cut-off, m (smax):          "'+e12+')', smax
  print, fo='("Equivalent area, km^2 (tau):             "'+f12+')', eatau
  print, fo='("Initial equivalent area, km^2 (tau):     "'+f12+')', eatau_init
  print, fo='("Initial cloud mass, 10^9 kg (Mcloud):    "'+f12+')', mcloud/1e9
  print, fo='("Impactor mass, 10^6 kg (Mimp):           "'+f12+')', mimp/1e6
  print, fo='("Impactor radius, m (Rimp):               "'+f12+')', rimp
  print, fo='("Target mass available, 10^6 kg (Mtarg1): "'+f12+')', mtarg1/1e6
  print, fo='("Impactor stream radius, km (Rstream):    "'+f12+')', rstream/1e3
  print, fo='("Target mass available, 10^12 kg (Mtarg2):"'+f12+')', mtarg2/1e12
endelse

if keyword_set(allclouds) then begin
  if keyword_set(twocx) then begin
    rem = (where( match[foo] eq 'Cx ', count ))[1]
    if count ne 2 then stop
    mcloud2 = mcloud[rem]
    mcloud = vec_remove( mcloud, rem )
    mtarg1 = vec_remove( mtarg1, rem )
    mtarg2 = vec_remove( mtarg2, rem )
    foo = vec_remove( foo, rem )
    nnn = nnn-1
  endif
  match[where( match eq 'Ax(1)' )] = 'Ax'
  plot, [-.5,nnn-.5], $
        [min([mcloud,mtarg1,mtarg2]),max([mcloud,mtarg1,mtarg2])], $
        /nodata, /xs, ys=8, /ylog, xticki=1, xtickn=match[foo], xtickle=1e-10, $
        xtit='Feature Name', $
        ytit='Inferred Cloud Mass (solid) or!CAvailable Target Mass (dashed), kg'
  xyouts, !x.crange[1] - (!x.crange[1]-!x.crange[0])*.02, $
          10^( !y.crange[1] - (!y.crange[1]-!y.crange[0])*.1 ), $
          (['(b)','(c)'])[allclouds-1], /align
  xyouts, 0, 1e12, 'q='+string(qq,fo='(I1)')
  solid_circles
  oplot, mcloud, ps=-8
  oplot, [rem-2,rem-1,rem], [ mcloud[rem-2], mcloud2, mcloud[rem] ], ps=-8
  open_circles
  oplot, mtarg1, ps=-8, l=2, color=ctred()
  oplot, mtarg2, ps=-8, l=2, color=ctgreen()
  axis, yaxis=1, ys=5, yr=(10^!y.crange/1e7*3/4/!pi)^(1./3), ytick_get=ytg
  ytg = ytg[where( ytg ge .01 and ytg le 10 )]
  ytn = strarr(n_elements(ytg))
  for k=0,n_elements(ytg)-1 do begin
    if ytg[k] lt 1 then begin
      ytn[k] = string( ytg[k], fo='(F'+strtrim(2-fix(alog10(ytg[k])),2)+$
                                   '.'+strtrim(-fix(alog10(ytg[k])),2)+')' )
    endif else begin
      ytn[k] = string( ytg[k], fo='(I'+strtrim(1+fix(alog10(ytg[k])),2)+')' )
    endelse
  endfor
  axis, yaxis=1, ys=1, yr=(10^!y.crange/1e7*3/4/!pi)^(1./3), $
        ytit='    Impactor Radius (m)', $
        ytickv=ytg, yticks=n_elements(ytg)-1, ytickn=ytn
endif else begin
  if keyword_set(paperplot) then begin
    if keyword_set(dolzr) then begin
      lzr, 'impactorsizecalcs', /half
      @plot_prepare
      plot_color
      device, /cmyk
    endif 
    !p.multi = [0,2,2]
  endif 
  if paperplot eq 1 then yma=[4-8,2+8] else yma=[4,2]
  plot, [min(qq)-0.25,max(qq)+0.25], yma=yma, $
        [1e6, 1e15], ys=9, $;[min(mcloud),max(mcloud)], $
        /nodata, /xs, /ylog, xtit='Size-distribution power-law index, q', $
        ;ytit='Cloud or Target Mass (kg)'
        ytit='Inferred Cloud Mass (solid) or!CAvailable Target Mass (dashed), kg'
  xyouts, !x.crange[1] - (!x.crange[1]-!x.crange[0])*.02, $
          10^( !y.crange[1] - (!y.crange[1]-!y.crange[0])*.1 ), '(a)', /align
  axis, yaxis=1, ys=1, yr=(10^!y.crange/1e7*3/4/!pi)^(1./3), $
        ytit='Impactor Radius (m)'
  for k=0,5 do begin
    if k eq 0 then th=!p.thick*4 else th=!p.thick
    open_circles
    if k eq 0 then oplot, qq[k*4:k*4+3], mtarg2[k*4:k*4+3], ps=-8, l=2, $
                          co=ctgreen()
    solid_circles
    oplot, qq[k*4:k*4+3], mcloud[k*4:k*4+3], ps=-8, thick=th
    xyouts, 2, 1e7, 'Feature "Ax"', chars=.8
    if k le 3 then begin
      case smax[k*4] of
        3: text = 's!Dmax!N = 3 m'
        .3: text = 's!Dmax!N = 30 cm'
        .03: text = 's!Dmax!N = 3 cm'
        .003: text = 's!Dmax!N = 3 mm'
      endcase 
      if k le 1 then xyouts, 2.05, mcloud[k*4]*1.5, text, chars=.8, orient=-17
      if k ge 2 and k le 3 then xyouts, 2.05, mcloud[k*4]*1.5, text, chars=.8, $
                                        orient=-20
    endif
    if k eq 0 or k ge 4 then begin
      case smin[k*4] of 
        3e-5: text = 's!Dmin!N = 30 !Mmm'
        3e-6: text = 's!Dmin!N = 3 !Mmm'
        3e-7: text = 's!Dmin!N = 0.3 !Mmm'
      endcase
      if k eq 4 then begin
        xyouts, 5, mcloud[k*4+3]*2, /align, text, chars=.8, orient=-13
      endif else begin
        xyouts, 5, mcloud[k*4+3]*2, /align, text, chars=.8, orient=-15
      endelse
    endif
  endfor 
  if keyword_set(paperplot) then begin
    allclouds = 1
    goto, plot1
  endif 
endelse
if keyword_set(paperplot) and allclouds eq 1 then begin
  allclouds = 2
  !p.multi[0] = !p.multi[0] - 1
  goto, plot1
endif
if keyword_set(dolzr) then clzr

end
