if not keyword_set(encke) and not keyword_set(keeler) and not keyword_set(aredge) then encke = 1

restore,'stretch.sav'
image_number =''
read,image_number,prompt='Image Number (0-'+strtrim(n_elements(filenames)-1,2)+') : '
image_number = fix(image_number)
image_name = filenames(image_number)

if not keyword_exists(nosmooth) then nosmooth = 0
if not keyword_set(screenx) then screenx = 1190

@caviar
; For 081/AZSCANDRK only
;if (where( encke eq image_number ))[0] ne -1 then begin
 ; encke = 1
 ; keeler = 0
;endif else begin
 ; encke = 0
 ; keeler = 0
 ; aredge = 1
;endelse

if keyword_set(keeler) then begin
  mnrad = 136460.0
  mxrad = 136550.0
  gap='K'
  inout = ['I','O']
endif else if keyword_set(encke) then begin
  mnrad = 133382.0
  mxrad = 133467.0
  gap = 'E'
  inout = ['I','O']
endif else if keyword_set(aredge) then begin
  mnrad = 136730.
  mxrad = 136800.
  gap = 'R'
  inout = ''
endif else stop, 'Please set either encke=1 or keeler=1 or aredge=1'
mnlon = 0
mxlon = 0
@r
rwin = !d.window

period = rstrpos( image_name, '.' )
filestem = strmid( image_name, 0, period )

for j=0,n_elements(inout)-1 do begin
  if j eq 1 and keyword_set(encke) then begin
    mnrad = 133705.
    mxrad = 133780.
    bke = 0
    @r
    rwin = !d.window
  endif
  print, 'Fit ' + gap + inout[j]
  @run_edgefit
  edge = j+1
  savefile = filestem + '.edge' + gap + inout[j]
  redge_cmat = cmat
  save, redge, redge_sigma, redge_cmat, filename=savefile
  dummy = redge_to_linsam_encke( image_name = image_name, gap=gap, edge=edge )
  if j ne n_elements(inout)-1 then wdelete, 8 else begin
    wdelete, rwin
    wdelete, 8
    wdelete, 1
  endelse
  if j eq 0 then begin
    window, 5
    !p.multi = [0,1,2]
  endif else begin
    wset, 5
    !p.multi = [1,1,2]
  endelse
  plot_nosci, redge[0,*], redge[1,*], /xs, /ys, $
              xtit='Longitude (!Uo!N)', ytit='Radius (km)'
  if j ne n_elements(inout)-1 then wset, rwin
endfor

print, strtrim(Image_Number,2) + ' / ' + strtrim(n_elements(filenames),2)

end
