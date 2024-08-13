qmin = 2
qmax = 6
dq = .01
qq = findgen((qmax-qmin)/dq+1)*dq + qmin
nq = n_elements(qq)
m_over_ea_rho = fltarr(nq)
smin = 1e-6
smax = 1.0
smin = 3e-6
smax = 0.03

foo = where( qq lt 3 )
;m_over_ea_rho[foo] = 4.0*(3-qq[foo])/3/(4-qq[foo])*smax
m_over_ea_rho[foo] = 4.0*(3-qq[foo])/3/(4-qq[foo]) * $
                     (smax^(4-qq[foo])-smin^(4-qq[foo])) / $
                     (smax^(3-qq[foo])-smin^(3-qq[foo]))
foo = where( qq eq 3 )
m_over_ea_rho[foo] = 4.0/3/alog(smax/smin)*smax
foo = where( qq gt 3 and qq lt 4 )
;m_over_ea_rho[foo] = $
;   4.0*(qq[foo]-3)/3/(4-qq[foo])*smin^(qq[foo]-3)*smax^(4-qq[foo])
m_over_ea_rho[foo] = 4.0*(3-qq[foo])/3/(4-qq[foo]) * $
                     (smax^(4-qq[foo])-smin^(4-qq[foo])) / $
                     (smax^(3-qq[foo])-smin^(3-qq[foo]))
foo = where( qq eq 4 )
m_over_ea_rho[foo] = 4.0/3*alog(smax/smin)*smin
foo = where( qq gt 4 )
;m_over_ea_rho[foo] = 4.0*(qq[foo]-3)/3/(qq[foo]-4)*smin
m_over_ea_rho[foo] = 4.0*(3-qq[foo])/3/(4-qq[foo]) * $
                     (smax^(4-qq[foo])-smin^(4-qq[foo])) / $
                     (smax^(3-qq[foo])-smin^(3-qq[foo]))

if keyword_set(dolzr) then begin
  psname = 'ejectamass'
  if keyword_set(fitline) then psname = psname + '_fitline'
  lzr, psname
  @plot_prepare
  if keyword_set(fitline) then plot_color
  !p.multi = [0,2,2]
endif
solid_small_circles
plot, qq, m_over_ea_rho, /ylog, $;ps=8, $
      ytit='M!Dcloud!N /( EA !Mr ), meters', $
      xtit='Ejecta Size-Distribution Power-Law Index, q'
x = .15
foo = where( qq lt 3-x or qq gt 4+x or ( qq gt 3+x and qq lt 4-x ) or qq eq 3 or qq eq 4 )
if keyword_set(fitline) then oplot, qq[foo], m_over_ea_rho[foo], co=ctred(), thick=3
oplot, !x.crange, [smin,smin], l=1
oplot, !x.crange, [smax,smax], l=1
xyouts, 2.5, smin*1.4, 's!Dmin!N', chars=1.5
xyouts, 5.15, smax/1.7, 's!Dmax!N', chars=1.5

if keyword_set(dolzr) then clzr

end
