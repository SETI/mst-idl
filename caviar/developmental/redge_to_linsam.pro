function redge_to_linsam, redge_cmat, redge, redge_sigma, savefile=savefile, $
  et=__et, nl=_nl, filestem=filestem, debug=debug, nosave=nosave, $
  noplot=noplot, redge_sigma_linsam=redge_sigma_linsam, $
  redge_dsigma_linsam=redge_dsigma_linsam

; Convert edgefit results given as input (or in savefile) back to
; image coordinates (line and sample).  For reverse, see linsam_to_redge.pro .

; Load saved edge if necessary
if keyword_set(savefile) then restore, savefile

; Load Spice kernels, and get polera and poledec
gspsilent = 1
@get_sat_prepare

; Find the ephemeris time
if keyword_set(__et) then et = __et else begin
  restore, 'stretch.sav'
  if not keyword_set(filestem) then filestem = strmid(savefile,0,11)
  jjj = (where( strmid(filenames,0,11) eq filestem, count ))[0]
  if count ne 1 then stop, 'Trouble finding jjj'
  restore, 'et.sav'
  et = _et[jjj]
endelse

; Find the size of the image in pixels
if keyword_set(_nl) then nl = _nl else begin
  if not keyword_set(jjj) then begin
    if keyword_set(_jjj) then jjj = _jjj else begin
      restore, 'stretch.sav'
    endelse
  endif
  restore, 'spreadsheet.sav'
  if not keyword_set(filestem) then filestem = strmid(savefile,0,11)
  kkk = (where( strmid(data[0,*],0,11) eq filestem, count ))[0]
  if count ne 1 then stop, 'Trouble finding kkk'
  case data[7,kkk] of 
    'FULL': nl = 1024
    'SUM2': nl = 512
    'SUM4': nl = 256
    else: stop, 'Trouble finding nl'
  endcase
endelse

; Convert from ring radius and longitude to RA and Dec
nredge = n_elements(redge[0,*])
get_ring, et, redge[1,*], 0, 0, polera, poledec, nredge, $
          edge, 699L, lons=redge[0,*], light_time=light_time

; Convert from RA and Dec to image coordinate, being sure to use the same
; C-matrix (redge_cmat) that was used to create redge in the first place.
vobs_planet = [0.0d0,0.0d0,0.0d0]
@get_cam_params
image_coords, edge, redge_cmat, vobs_planet, cam_params, nl, redge_linsam

; Convert redge_sigma to a useful error bar for redge_linsam
get_ring, et, redge[1,*]-redge_sigma[1,*], 0, 0, polera, poledec, nredge, $
          edge_minus, 699L, lons=redge[0,*], light_time=light_time
get_ring, et, redge[1,*]+redge_sigma[1,*], 0, 0, polera, poledec, nredge, $
          edge_plus, 699L, lons=redge[0,*], light_time=light_time
image_coords, edge_minus, redge_cmat, vobs_planet, cam_params, nl, $
              redge_minus_linsam
image_coords, edge_plus, redge_cmat, vobs_planet, cam_params, nl, $
              redge_plus_linsam
redge_sigma_linsam = rebin([ [[abs( redge_plus_linsam - redge_linsam )]], $
                             [[abs( redge_minus_linsam - redge_linsam)]] ],$
                           nredge,2)
redge_dsigma_linsam = abs( abs(redge_plus_linsam-redge_linsam) - $
                           redge_sigma_linsam )
redge_linsam = rotate( redge_linsam, 4 )
redge_sigma_linsam = rotate( redge_sigma_linsam, 4 )
redge_dsigma_linsam = rotate( redge_dsigma_linsam, 4 )

if not keyword_set(nosave) then begin
  save, redge_cmat, redge_linsam, redge_sigma_linsam, redge_dsigma_linsam, $
        filename=savefile+'_linsam'
  print, 'Saved edge line/sample to '+savefile+'_linsam'
endif

if not keyword_set(noplot) then begin
  plots, redge_linsam[1,*], nl-redge_linsam[0,*], /device, ps=3, color=cyan()
  print, 'Plotted to window '+strtrim(!d.window,2)
endif

if keyword_set(debug) then stop

return, redge_linsam

end
