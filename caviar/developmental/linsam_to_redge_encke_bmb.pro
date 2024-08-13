;This is a modified version of linsam_to_redge_encke.
;Written by B. Byington Spring 2011
;I ran into difficulties coding a dynamic et / det _et into the
;Cassini bootstrap code (bootstrap_redge_cassini_bmb)
;because when linsam_to_redge_encke is called,
;any new version of et was overwritten. My solution was to replace
;all reference to linsam_to_redge_encke in boostrap_redge_cassini_bmb
;with this version, linsam_to_redge_encke_bmb. I tried to add dynamic
;to this code but it didn't work. Instead the only way I've found that
; works well is to add or subract a value (in seconds) lower in this
; code where it says XX. This unfortunately requires the user to
; re-start idl whenever a new et offset is investigated but was the
; simplest way to fix the problem. This code seemed awfully buggy from
; the beginning. It runs through the same parts multiple times and I
; (B. Byington) have never fully understood what's going on here.

function linsam_to_redge_encke_bmb, _redge_cmat, redge_linsam, redge_sigma_linsam, $
  redge_dsigma_linsam, savefile=savefile, nl=_nl, filestem=filestem, $
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
  ;reply5=''
  ;print, 'Enter et offset in seconds'
  ;read, reply5
  ;reply5=float(reply5)
  ;b=reply5
  ;print, b
  ;print, "You are here"
  et = _et[jjj];+XX
  print, et
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
