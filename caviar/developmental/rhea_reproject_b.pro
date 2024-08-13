print, 'Getting z-r array...'
zra = zrarray1( et, nl, cam_params, cmat, polera, poledec, sc, /rhea, ll=ll )
zra[0,*,*] = zra[0,*,*] / 764  ; Convert from km to R_Rhea

; Line of increasing r (constant z)
rrpts = wher( abs(zra[1,*,*]) lt 1 )
rrfit = poly_fit( rrpts[1,*], rrpts[2,*], 1 )
rrsamp = findgen(nl)
rrline = poly( rrsamp, rrfit )
rrang = atan( -rrfit[1] )*180/!pi
dz = 100 ;pixels
rrline_up = rrline + dz/cos(rrang*!pi/180)
rrline_down = rrline - dz/cos(rrang*!pi/180)
if not keyword_set(rhea_rpj_dr) then rhea_rpj_dr = 0.005
if not keyword_set(rhea_rpj_rmin) then rhea_rpj_rmin = 1.5;1.25
;if not keyword_set(rhea_rpj_rmin) then begin
;  if _keywords[jjj].ringplane_aimpoint_phase_angle gt 100 then begin
;    rhea_rpj_rmin = 1.25
;  endif else begin
;    rhea_rpj_rmin = 1.5
;  endelse
;endif
if not keyword_set(rhea_rpj_rmax) then rhea_rpj_rmax = 6
;rr = findgen( (rhea_rpj_rmax-rhea_rpj_rmin)/rhea_rpj_dr + 1 )*rhea_rpj_dr + $
;     rhea_rpj_rmin
rr = findgen( 901 )*rhea_rpj_dr + rhea_rpj_rmin ;otherwise varying rhea_rpj_rmin will $
;      give wrong array dimensions
if keyword_set(doplot) then begin
  loadct, 15
  tvscl, /order, abs(zra[1,*,*])
  plots, /device, rr[1,*], 1024-rr[2,*], ps=3, color=green()
  plots, /device, rrsamp, 1024-rrline, color=yellow()
  plots, /device, rrsamp, 1024-rrline_up, color=yellow()
  plots, /device, rrsamp, 1024-rrline_down, color=yellow()
endif

sm = 19 ;25 ;21 ;15
marg = 8

;reply1 = ''
;while reply1 eq '' do begin
;    print, 'sm,      marg,     image_name'
;    print, sm, marg
;    print, image_name
;    print, 'Adjust [s]m, [m]arg, [b]oth (sm first), or keep [o]riginal values.'
;    read, reply1
;    case reply1 of
;        's': begin
;            print, 'Enter sm value.'
;            reply2 = ''
;            read, reply2
;            reply2 = float(reply2)
;            sm = reply2
;        end
;        'm': begin       
;            print, 'Enter marg value.'
;            reply2 = ''
;            read, reply2
;            reply2 = float(reply2)
;            marg = reply2
;        end
;        'b': begin
;            print, 'Enter sm value.'
;            reply2= ''
;            read, reply2
;            reply2 = float(reply2)
;            sm = reply2
;            print, 'Enter marg value.'
;            reply3 = ''
;            read, reply3
;            reply3 = float(reply3)
;            marg = reply3
;        end
;        'o': begin
;            sm = 15
;             marg = 8
;         end
;
;        else: reply1= ''
;    endcase
;endwhile

rawim = sigma_filter( rawim, sm, n_sigma=3, /all )
print, sm
rawim = rawim - smooth(rawim,sm,/edge)
if keyword_set(cray) then begin
  ; Zero points within sm pixels of a cray
  cray_filter = bytarr(nl,nl) + 1
  for j=-sm,sm do for k=-sm,sm do cray_filter[cray[*,1]+j,cray[*,0]+k] = 0
  rawim = rawim * cray_filter
endif

;imrz = fltarr( (rhea_rpj_rmax-rhea_rpj_rmin)/rhea_rpj_dr + 1, dz*2+1 )
imrz = fltarr( 901, dz*2+1 );otherwise varying rhea_rpj_rmin will $
     ; give wrong array dimensions
print, 'Reprojecting image...'
if not keyword_set(fac) then fac = 1
print, 'fac = '+strtrim(fac,2)
for j=-dz,dz do begin
  _rrfit = rrfit
  _rrfit[0] = _rrfit[0] + j/cos(rrang*!pi/180) * fac
  _rrline = poly( rrsamp, _rrfit )
  _rr = interpolate( zra[0,*,*], rrsamp, _rrline, missing=-1 )
  foo1 = where( _rr ne -1, count1 )
  if count1 ne 0 then begin
    __rrsamp = interpol( rrsamp[foo1], _rr[foo1], rr )
    __rrline = poly( __rrsamp, _rrfit )
    if j mod 20 eq 0 then plots, /device, __rrsamp, 1024-__rrline, color=green()
    foo2 = where( __rrsamp ge rrsamp[foo1[0]] and $
                  __rrsamp le rrsamp[foo1[count1-1]] )
    imrz[foo2,j+dz] = interpolate( rawim, __rrsamp[foo2], __rrline[foo2], $
                                   missing=0, cubic=-0.5 )
    ; Zero points within sm pixels of image edge
    foo3 = where( __rrsamp lt sm+marg or __rrsamp gt nl-sm-marg or $
                  __rrline lt sm+marg or __rrline gt nl-sm-marg, count3 )
    if count3 ne 0 then imrz[foo3,j+dz] = 0
  endif 
endfor

end
