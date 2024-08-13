restore, 'prop_reproj_cea.sav'
restore, 'calculate_ea.sav'

j = 1
xy = 0

rrpi = *( prop_reproj[j].rrpi )
sz = size(rrpi)
foo = wher( rrpi eq 0, count )
mask = bytarr( sz[1], sz[2] ) + 1
box = 1
for k=0l,count-1 do begin
  mask[ (foo[0,k]-box)>0 : (foo[0,k]+box)<(sz[1]-1), $
        (foo[1,k]-box)>0 : (foo[1,k]+box)<(sz[2]-1) ] = 0
endfor
rrpi[where( mask eq 0 )] = median(rrpi[where( mask eq 1 )])
mnrad = prop_reproj[j].radlon[0]
mxrad = prop_reproj[j].radlon[1]
mnlon = prop_reproj[j].radlon[2]
mxlon = prop_reproj[j].radlon[3]
radx = make_radi( mnrad, mxrad, mnlon, mxlon, sz, loni=lonx )

if keyword_set(dolzr) then begin
  if dolzr ge 2 then begin
    lzr, 'calculate_ea_width0'
    @plot_prepare
  endif
endif

!p.multi = [0,2,3]
!y.margin = 0
!y.omargin = [4,2]
!p.charsize = 1.5
notn = replicate(' ',20)
foo = where( *yind[j,xy] ge center_range[j,xy,0] and $
             *yind[j,xy] le center_range[j,xy,1]+5 )
;plot, tkm(radx[*yind[j,xy]]), *ew[j,xy], /xs
;plot, tkm(radx[*yind[j,xy]]), *width[j,xy], /xs
;oplot, (tkm(radx[*yind[j,xy]]))[foo], (*width[j,xy])[foo], co=red()
plot, (tkm(radx[*yind[j,xy]]))[foo], (*ew[j,xy])[foo], /xs, /ynozero, $
      ytit='Equivalent Width (km)', xtickn=notn
oplot, !x.crange, [0.1,0.1], l=1
!p.multi[0] = !p.multi[0] - 1
plot, (tkm(radx[*yind[j,xy]]))[foo], (*width[j,xy])[foo], /xs, /ynozero, $
      ytit='Gaussian Width Parameter (km)', xtickn=notn
!p.multi[0] = !p.multi[0] - 1

rr = (radx[*yind[j,xy]])[foo]
_ew = (*ew[j,xy])[foo]
_sigma = (*width[j,xy])[foo]
i0 = 0.1

ww = 2.0d0 * sqrt(2*alog(_ew/i0)) * _sigma
plot, tkm(rr), ww, /xs, xtit='Radius'+tkmtit(), $
      ytit='Azimuthal Width at I='+string(i0,fo='(F3.1)')+' km'
theta = findgen(361)*!pi/180
for radius=200.,260,20 do oplot, mean(!x.crange)+radius/2000*cos(theta), $
                                radius*sin(theta), l=1
print, 'The reason these plots were confusing is that the actual shifted feature is not round or oval-shaped but squarish.  Only a fraction of the x-axis in the plots corresponds to the oval-shaped feature seen in the image of the shifted feature.  The widths seen in the shifted feature fit with those seen in the calculate_ea.pro profiles, when you convert from pixels to km.'

if keyword_set(dolzr) then begin
  if dolzr ge 2 then clzr
  lzr, 'calculate_ea_width0_imdisp', /half
  @plot_prepare
;  plot_color
  device, decomposed=0
endif

rrpi_shift = rrpi
;slopefit[j,xy,1] = 1/slopefit[j,1-xy,1];slopefit[j,xy,1] - 1.5
slope = poly( indgen(sz[2]), slopefit[j,xy,*] )
for k=0,sz[2]-1 do begin
  rrpi_shift[*,k] = shift( rrpi[*,k], slope[sz[2]/2]-slope[k] )
endfor
if !d.name eq 'X' then window, 1
!p.multi = 0

;lonxnorm = (lonx-275.93717)*!pi/180*mean(radx)
lonxnorm = (lonx-275.945)*!pi/180*mean(radx)
xtit = 'Relative Azimuthal Length (km)'
ytit = 'Orbital Radius '+tkmtit()

x0 = min(where( lonxnorm gt -650 ))
x1 = max(where( lonxnorm lt 550 )) 
y0 = min(where( radx gt 129120 ))
y1 = max(where( radx lt 129620 )) 
if !d.name eq 'X' then begin
  imdispim = rrpi_shift[x0:x1,y0:y1]>.00105<.002
endif else imdispim = (.002-rrpi_shift[x0:x1,y0:y1])>0<.00095
imdisp, imdispim, /axis, xtit=xtit, ytit=ytit, $
        xr=[lonxnorm[x0],lonxnorm[x1]], yr=tkm([radx[y0],radx[y1]])

if keyword_set(dolzr) then begin
  clzr
  stop
  lzr, 'calculate_ea_width0_contour', /half
  @plot_prepare
;  plot_color
  device, decomposed=0
endif else stop
;contour, rrpi_shift, lonx, tkm(radx), /xs, /ys, xr=[275.65,276.2]

;contour, rrpi_shift, lonx*!pi/180*mean(radx), radx, xs=5, ys=5, /iso, $
;         xr=[6.223e5,6.236e5], yr=[1.292e5,1.2963e5]
;axis, xaxis=0, /xs, xr=!x.crange/mean(radx)*180/!pi, xtit='Longitude (!Uo!N)'
;axis, xaxis=1, /xs, xr=!x.crange/mean(radx)*180/!pi, xtickn=notn
;axis, yaxis=0, /ys, yr=tkm(!y.crange), ytit='Radius '+tkmtit()
;axis, yaxis=1, /ys, yr=tkm(!y.crange), ytickn=notn

;contour, rrpi_shift, lonxnorm, radx-129370, $
;         xtit='Azimuthal Length (km)', ytit='Radial Length (km)', $
;         /xs, /ys, /iso, xr=[-600,600], yr=[-250,250]

mnp = 0.0011;0.001
mxp = 0.002
dlv = 0.0001
cc1 = 0
cc2 = 255
c_colors = cc2-findgen((mxp-mnp)/dlv+1)/((mxp-mnp)/dlv)*(cc2-cc1)
levels = findgen((mxp-mnp)/dlv+1)*dlv+mnp
;contour, rrpi_shift, lonxnorm, radx, $
;         xtit=xtit, /xs, ys=5, /iso, /fill, $
;         xr=[-600,600], yr=[129120,129620], $
;         levels=findgen(20)/20000 + .0008 ;, nlevels=5, levels=levels
contour, rrpi_shift, lonxnorm, radx, $
         xtit=xtit, /xs, ys=5, /iso, /fill, $
         xr=[-600,600], yr=[129120,129620], c_labels=replicate(1,10), $
         levels=levels, c_colors=c_colors
;contour, rrpi_shift, lonxnorm, radx, $
;         xtit=xtit, /xs, ys=5, /iso, $
;         xr=[-600,600], yr=[129120,129620], c_labels=replicate(1,10)
;contour, rrpi_shift, lonxnorm, radx, $
;         xtit=xtit, /xs, ys=5, /iso, /fill, $
;         xr=[-600,600], yr=[129120,129620]
axis, yaxis=0, /ys, yr=tkm(!y.crange), ytit=ytit
axis, yaxis=1, /ys, yr=tkm(!y.crange), ytickn=notn

if keyword_set(dolzr) then clzr

end
