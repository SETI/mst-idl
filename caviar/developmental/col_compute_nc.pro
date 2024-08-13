PRO col_compute_nc,image_name,im,stars,x,y,x_streaks,y_streaks,star_col_tab,x_col,y_col,max_streak_dn
buffer=3.

;srloc=strpos(image_name,'_cal.')
;raw_image_name=strmid(image_name,0,srloc)+'.IMG'
;uncalim=read_vicar(raw_image_name)
; njc 02nov08:
rawim=im
uncalim=rawim
nl=n_elements(rawim(0,*))

star_names,stars,s_names
smags=string(stars[*,3]/100.0, format='(f5.2)')

nux=n_elements(x)
xx_streaks=x_streaks-rebin(x_streaks(4,*),9,nux)+rebin(reform(x,1,nux),9,nux)
yy_streaks=y_streaks-rebin(y_streaks(4,*),9,nux)+rebin(reform(y,1,nux),9,nux)
xarr=(fltarr(nl)+1.)##findgen(nl)
yarr=(fltarr(nl)+1.)#findgen(nl)

x_col=fltarr(nux)
y_col=fltarr(nux)
max_streak_dn=fltarr(nux)
for i=0,nux-1 do begin
if min(xx_streaks(*,i)) gt 10 and max(xx_streaks(*,i)) lt nl-11 $
	and min(yy_streaks(*,i)) gt 10 and max(yy_streaks(*,i)) lt nl-11 $
	then begin
tarr=fltarr(nl,nl)
qq=where(xarr gt min(xx_streaks(*,i))-buffer and $
	xarr lt max(xx_streaks(*,i))+buffer and $
	yarr gt min(yy_streaks(*,i))-buffer and $
	yarr lt max(yy_streaks(*,i))+buffer)
tarr(qq)=1.
qq2=where(xarr gt min(xx_streaks(*,i))-2*buffer and $
	xarr lt max(xx_streaks(*,i))+2*buffer and $
	yarr gt min(yy_streaks(*,i))-2*buffer and $
	yarr lt max(yy_streaks(*,i))+2*buffer and tarr eq 0)

rb=median(rawim(qq2))
rx=rawim-rb
max_streak_dn(i)=max(uncalim(qq))
x_col(i)=total(xarr(qq)*rx(qq))/total(rx(qq))
y_col(i)=total(yarr(qq)*rx(qq))/total(rx(qq))
end
end

qz=where(max_streak_dn ne 0)
star_col_tab=strarr(5, n_elements(qz))
star_col_tab(0,*)=s_names(qz)
star_col_tab(1,*)=smags(qz)
star_col_tab(2,*)=string(y_col(qz), format='(f7.2)')
star_col_tab(3,*)=string(x_col(qz), format='(f7.2)')
star_col_tab(4,*)=string(max_streak_dn(qz), format='(f6.0)')


end

