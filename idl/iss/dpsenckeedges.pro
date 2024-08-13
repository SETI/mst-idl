;for j=0,1 do begin
j=0

  if j eq 0 then begin
    restore, '/home/borogove/iss/images/mmhedman/8lphrlf_proc_may20.sav'
    zz=out
  endif else begin
    restore, '/home/borogove/iss/images/mmhedman/LPHRLF_proc_may9.sav'
    zz[0,*]=zz[0,*]-360
  endelse

  xr=[-30,5];[-127.27,-40]
  zzr = where( zz[0,*] ge xr[0] and zz[0,*] le xr[1] )
  baseline25 = smooth(zz[25,*],60)
  baseline26 = smooth(zz[26,*],60)
  !p.multi=[0,1,3]
  !p.charsize=3

  oldomargin=!y.omargin
  !y.omargin=[4,2]
  plot, zz[0,*], zz[26,*]-baseline26, /xs, /ys, xr=xr, yr=[-2.5,2.5], $
	yma=[0,0], xtickn=replicate(' ',20), $
	ytit='Radial Distance from!C'+strtrim(long(mean(baseline26[zzr])),2)+$
	' km'
  oplot, [0,0], !y.crange, l=1
  plot, zz[0,*], zz[25,*]-baseline25, /xs, /ys, xr=xr, yr=[-2.5,2.5], $
	yma=[0,0], xtit='Longitude Relative to Pan', $
	ytit='Radial Distance from!C'+strtrim(long(mean(baseline25[zzr])),2)+$
	' km'
  oplot, [0,0], !y.crange, l=1

;endfor

end
