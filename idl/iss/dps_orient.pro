; Make the orientation image for my 2004 DPS Poster.
!p.font = 1
!p.charsize = 4
!p.thick = 4
white = 1

im = read_vicar( '/home/adeona/images/00A/COMPHILIT/W1477740059_1_cal.IMG' )
im = rotate( im, 3 )
im = im[*,550:850]
sz = size(im)

wsy = 800.
window, xs=1024, ys=wsy
tvscl, im, 0, wsy-sz[2]
if keyword_set(white) then tv,intarr(1024,wsy-sz[2])+255

solid_diamonds
num = [ 4., 3, 1, 6, 5, 2 ]
head = interpol( indgen(1024), radarray[474,*], [118800,128100,128950,132550,133100,136400] )
;head = [ 561., 756, 770, 844, 856, 925 ]
tail = findgen(6)*50+650
;plots, /normal, ps=8, head/1024, replicate(wsy-sz[2]-5,6)/wsy
for j=0,5 do begin
  arrow, tail[j]/1024, (wsy-sz[2]-80)/wsy, head[j]/1024, (wsy-sz[2]-5)/wsy, /normalized, hthick=4, thick=4, hsize=16, /solid, co=255*(1-white)
endfor
xyouts, /normal, tail/1024, (replicate(wsy-sz[2]-120,6))/wsy, strtrim(fix(num),2), align=.5, co=255*(1-white)

if keyword_set(white) then wh='white' else wh=''
tifffile = '~/Desktop/dps_orient'+wh+'.tiff'
;.run caviar_tiff

end
