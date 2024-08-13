; B. Byington created this code as a modified version of
; redge_to_linsam_encke.pro as part of the way to dynamically change
; the ephemeris time, but ended up never really using it. It could
; be useful in the future if someone finds a neater way to change et
;
;Original of redge_to_linsam_encke.pro
function redge_to_linsam_encke, redge_cmat, redge, redge_sigma, $
  et=__et, nl=_nl, debug=debug, noplot=noplot, $
  redge_sigma_linsam=redge_sigma_linsam, $
  redge_dsigma_linsam=redge_dsigma_linsam, image_name=image_name, $
  edge = edge, gap = gap

; Convert edgefit results given as input (or in savefile) back to
; image coordinates (line and sample).  For reverse, see linsam_to_redge.pro .

;------------------- LOADING .EDGE FILE -------------;

period = rstrpos( image_name, '.' )
filestem = strmid(image_name,0,period)

if (gap eq 'E' or gap eq 'e') then begin
  if edge eq 1 then begin
    savefile = filestem+'.edgeEI'
    dumpfile = filestem+'.edge_linsamEI'
  endif
  if edge eq 2 then begin
    savefile = filestem+'.edgeEO'
    dumpfile = filestem+'.edge_linsamEO'
  endif
endif

if (gap eq 'K' or gap eq 'k') then begin
  if edge eq 1 then begin
    savefile = filestem+'.edgeKI'
    dumpfile = filestem+'.edge_linsamKI'
  endif
  if edge eq 2 then begin
    savefile = filestem+'.edgeKO'
    dumpfile = filestem+'.edge_linsamKO'
  endif
endif

if (gap eq 'R' or gap eq 'r') then begin
  savefile = filestem+'.edgeR'
  dumpfile = filestem+'.edge_linsamR'
endif

restore,savefile

;------------------- Working Code ---------------------;

; Find the ephemeris time
if keyword_set(__et) then et = __et else begin
  restore, 'stretch.sav'
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
  kkk = (where( strmid(data[0,*],0,11) eq strmid(filestem,0,11), count ))[0]
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
@get_cam_params1
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

;------------------ SAVING ----------------------;
print,'Saving...'

  save, redge_cmat, redge_linsam, redge_sigma_linsam, redge_dsigma_linsam, $
        filename=dumpfile
  print, 'Saved edge line/sample to'+dumpfile

return, redge_linsam

end
