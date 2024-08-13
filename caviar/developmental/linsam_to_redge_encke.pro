function linsam_to_redge_encke, _redge_cmat, redge_linsam, redge_sigma_linsam, $
  redge_dsigma_linsam, savefile=savefile, et=__et, nl=_nl, filestem=filestem, $
  debug=debug, nosave=nosave, noplot=noplot, redge1_sigma=redge1_sigma

; Convert edgefit results given as image coordinates (line and sample) to 
; radius and longitude.  The reverse of redge_to_linsam.pro .  

; Possibly use a different cmat from the original
if keyword_set(_redge_cmat) then redge_cmat = _redge_cmat

; Find the ephemeris time
if keyword_set(__et) then et = __et else begin
  restore, 'stretch.sav'
  if not keyword_set(filestem) then filestem = strmid(savefile,0,11)
  jjj = (where( strmid(filenames,0,11) eq strmid(filestem,0,11), count ))[0]
  if count ne 1 then stop, 'Trouble finding jjj'
  restore, 'et.sav'
  et = _et[jjj]
endelse

; Load Spice kernels, and get polera and poledec
gspsilent = 1
@get_sat_prepare1

; Find the size of the image in pixels
if keyword_set(_nl) then nl = _nl else begin
  if not keyword_set(jjj) then begin
    if keyword_set(_jjj) then jjj = _jjj else begin
      restore, 'stretch.sav'
    endelse
  endif
  restore, 'spreadsheet.sav'
  if not keyword_set(filestem) then filestem = strmid(savefile,0,11)
  kkk = (where( strmid(data[0,*],0,11) eq strmid(filestem,0,11), count ))[0]
  if count ne 1 then stop, 'Trouble finding kkk'
  case data[7,kkk] of 
    'FULL': nl = 1024
    'SUM2': nl = 512
    'SUM4': nl = 256
    else: stop, 'Trouble finding nl'
  endcase
endelse

; Convert from image coordinates to RA and Dec
nredge = n_elements(redge_linsam[0,*])
@get_cam_params1
p2radec, cam_params, redge_cmat, nl, redge_linsam[0,*], $
         redge_linsam[1,*], RA, dec

; Convert from RA and Dec to radius and longitude
p2ralon, redge_cmat, et, polera, poledec, sc, RA, dec, rad, lon
redge1 = [ lon, rad ]

; Convert redge_sigma_linsam back to error bar on radius
redge1_sigma = [ lon, rotate(v_mag( rotate(redge_sigma_linsam,4) ),1) * $
                 _keywords[jjj].ringplane_aimpoint_radial_scale[0] ]

if keyword_set(debug) then stop

return, redge1

end
