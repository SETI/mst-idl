; Written by M. Hedman.  Slightly modified by M. Tiscareno
PRO shadowedge_mst, sun_lat, sun_lon, shad_lon, shad_radi, a=a, b=b
       tb=tan(abs(sun_lat)*!dpi/180.)
if not keyword_set(a) then a=60268.;keywords, old b restores 112410
if not keyword_set(b) then b=54364.; 54700.;54364.
;	b=54364.
       y=findgen(60250*2)-60250. ;fixed 60520 080312
       ap=sqrt(a^2-y^2)
       tanbp=tb*(a/b)
       sinbp=tanbp/sqrt(1+tanbp^2)
       x=-ap/sinbp
       ;x2=(1-y^2/a^2)*(tb+1/tb)^2/(tb^2/a^2+1./b^2)
       ;x=-sqrt(x2)
       shad_radi=sqrt(x^2+y^2)
       shad_lon=(atan(y,x)*180./!Pi+sun_lon+720+180) mod 360 -180
       end
