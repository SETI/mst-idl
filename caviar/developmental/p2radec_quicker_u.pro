
pro p2radec_quicker_u,pixscale,cmat,nl,line,sample,RA,dec

; This method is now not only quicker but more accurate than older methods.
; It simply inverts the process performed in image_coords.pro, linearizing
; the cubic equation by making use of the fact that cp5 and cp6 are zero,
; while cp1, cp2, and cp3 are very small, on the order of 1e-5. 
; (Note: By cp1, I mean cam_params[1], and so on.)

;if (where(cam_params[[5,6]] ne 0))[0] ne -1 then stop, 'Camera parameters have changed, and this method is no longer valid.  Please use an older version of p2radec.'
;if (where(abs(cam_params[1:3]) gt 1e-2))[0] ne -1 then stop, 'Camera parameters have changed, and this method is no longer valid.  Please use an older version of p2radec.'

szx = size(line)
szy = size(sample)
if (where(szx ne szy))[0] ne -1 then stop, szx, szy
nn = szx[szx[0]+2]
if nn gt 1 then begin
  line = reform(line,nn)
  sample = reform(sample,nn)
endif



x0 = (sample-nl/2)
y0 = (line-nl/2)
; Relative to optical center of distortion
;x0oc = x0 - cam_params[8]/cam_params[4]*summ
;y0oc = y0 - cam_params[9]/cam_params[4]*summ
;x = x0*0
;y = y0*0
;xz = where( x0 eq 0, countx )
;xnz = where( x0 ne 0, countxn )
;if countx ne 0 then begin
;  for jj=0,countx-1 do begin
;    if y0[xz[jj]] eq 0 then begin
;      x[xz[jj]] = x0[xz[jj]]  ;0
;      y[xz[jj]] = y0[xz[jj]]  ;0
;    endif else begin
;      c = x0[xz[jj]] / y0[xz[jj]]  ;0
;      deltay = - ( (c*cam_params[3]+cam_params[2])*y0[xz[jj]]^2 + $
;                   cam_params[1]*(c^2+1)*y0[xz[jj]]^3 ) / $
;               ( 1 + (c*cam_params[3]+cam_params[2])*2*y0[xz[jj]] + $
;                 cam_params[1]*(c^2+1)*3*y0[xz[jj]]^2 )
;      y[xz[jj]] = y0[xz[jj]] + deltay
;      x[xz[jj]] = c * y[xz[jj]]
;    endelse
;  endfor
;endif
;if countxn ne 0 then begin
;  b = y0[xnz] / x0[xnz]
;  deltax = - ( (cam_params[3]+b*cam_params[2])*x0[xnz]^2 + $
;               cam_params[1]*(b^2+1)*x0[xnz]^3 ) / $
;           ( 1 + (cam_params[3]+b*cam_params[2])*2*x0[xnz] + $
;             cam_params[1]*(b^2+1)*3*x0[xnz]^2 )
;  x[xnz] = x0[xnz] + deltax
;  y[xnz] = b * x[xnz]
;endif
;deltay = - ( cam_params[3]*x0oc*y0oc+cam_params[2]*y0oc^2 + $
 ;            cam_params[1]*(x0oc^2*y0oc+y0oc^3) ) / $
  ;       ( 1 + cam_params[3]*2*x0oc+cam_params[2]*2*y0oc + $
   ;        cam_params[1]*3*(x0oc^2+y0oc^2) )
;deltax = - ( cam_params[3]*x0oc^2+cam_params[2]*y0oc*x0oc + $
 ;            cam_params[1]*(x0oc^3+y0oc^2*x0oc) ) / $
  ;       ( 1 + cam_params[3]*2*x0oc+cam_params[2]*2*y0oc + $
   ;        cam_params[1]*3*(x0oc^2+y0oc^2) )
x = x0 ;+ deltax
y = y0 ;+ deltay

rho=dblarr(nn,3)
;rho[*,0]=(sample-boresight[0])/(cam_params[0]*cam_params[4]/summ)
;rho[*,1]=(line-boresight[1])/(cam_params[0]*cam_params[7]/summ)
rho[*,0] = (sample-nl/2)*pixscale 
rho[*,1] = (line-nl/2)*pixscale
rho[*,2] = 1.0d0

rho=rho/rebin(v_mag(rho),nn,3)

tcmat=transpose(cmat)

;if n_elements(tcmat) eq 9 then cspice_mtxv, cmat, reform(rho(0,*)), d_rho_j2000 else begin
  d_rho_j2000 = dblarr(nn,3)
  for j=0,nn-1 do begin
	cspice_mtxv, cmat, reform(rho[j,*]), d_rho_j2000x
    d_rho_j2000[j,*] = d_rho_j2000x
  endfor 
;endelse

radec = cart_to_polar(d_rho_j2000)
foo = where( radec lt 0, count )
if count gt 0 then radec[foo] = radec[foo] + 2*!dpi

RA = radec[*,1] * 360.0d0/(2.0d0*!dpi)
dec = radec[*,0] * 360.0d0/(2.0d0*!dpi)

if nn gt 1 then begin
  if szx[0] eq 2 then begin
    RA = reform( RA, szx[1], szx[2] )
    dec = reform( dec, szx[1], szx[2] )
  endif
endif

end

