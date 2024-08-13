function dw_to_rres, redge1, redge2, redge3, rres, redge1_sigma=redge1_sigma, redge2_sigma=redge2_sigma, redge3_sigma=redge3_sigma, tit=tit

if n_params() eq 0 then begin
  print, 'Syntax:  redge_rres = DW_TO_RRES( redge1, redge2, redge3, rres )'
  retall
endif

if !d.name eq 'X' then window, 14
!p.multi=[0,2,1]
sm = 500

if not keyword_set(tit) then tit=''
foo = [ redge1[1,*], redge2[1,*], redge3[1,*] ]
yr1 = tkm([ min(foo), max(foo) ],thoukm=thoukm)
plot_redge,redge1,redge1_sigma,yr1=yr1,tit=tit,keywords=keywords
l1 = redge1[0,*]
r1 = redge1[1,*]
plot_redge,redge2,redge2_sigma,/oplot
l2 = redge2[0,*]
r2 = redge2[1,*]
plot_redge,redge3,redge3_sigma,/oplot
l3 = redge3[0,*]
r3 = redge3[1,*]

ll = [ reform(l1), reform(l2), reform(l3) ]
ll = ll[sort(ll)]
; Include only longitude values common to all three curves
nll = n_elements(ll)
foo = where( ll[2:nll-1] eq ll[0:nll-3], count )
if count gt 0 then ll = ll[foo] else stop, 'No common longitudes'

; Resample the curves to a common longitude abscissa
r1r = interpol( r1, l1, ll )
r2r = interpol( r2, l2, ll )
r3r = interpol( r3, l3, ll )

;clzr

delta_rres = ( 2*(r2r-rres)^2 - (r1r-rres)^2 - (r3r-rres)^2 ) / $
              (2*r2r-r1r-r3r) / 2
plot, ll, tkm( rres+delta_rres, thoukm=thoukm ), /xs, ys=9, xtit='Longitude (deg)', $
      ytit='Resonance location'+tkmtit( rres+delta_rres, thoukm=thoukm ), $
      tit='Nominal rres = '+strtrim(rres,2)+' km', xma=[10,10]
axis, yaxis=1, /data, /ys, ytit='Delta_rres (km)', yr=[min(delta_rres),max(delta_rres)]

redge = [ rotate(ll,1), rotate(rres+delta_rres,1) ]
return, redge
;keep_redge = 1
;.run repoint

end
