; The first curve came from Joe Spitale.  The second came from my old edgefit,
; which projects the image into radius and longitude, fits the edge, then 
; projects back into image coordinates.  The third (and all further) came from 
; a new edgefit which fits the edge in terms of image coordinate with 
; no reference to geometry kernels.  The fourth came from an edgefit that 
; fit the peak with a quadratic rather than a gaussian. 
filenames = [ 'N1493613276_spitale.edge', $
              'N1493613276_1_cal.edge'+strtrim(sindgen(12)+1,2)+'_linsam' ]
descrip = [ 'Spitale', $
            'Tiscareno (gaussian 1)', $  ; Old edgefit
            'Tiscareno (gaussian 2)', $  ; Same as g1, no geometry kernels
            'Tiscareno (quadratic 1)', $ ; Same as g2, q-fit to 4 or 5 points
            'Tiscareno (quadratic 2)', $ ; Same as q1, points gt half peak
            'Tiscareno (quadratic 3)', $ ; caviar_edgefind
            'Tiscareno (gaussian 3)', $  ; caviar_edgefind
            'Tiscareno (quadratic 4)', $ ; caviar_edgefind, /gaussfilt
            'Tiscareno (quadratic 5)', $ ; caviar_edgefind, /sincfilt
            'Tiscareno (quadratic 6)', $ ; edgefit, r w/ reproj_sinc=1, delta=10
            'Tiscareno (gaussian 5)' , $ ; edgefit, r w/ reproj_sinc=1, delta=10
            'Tiscareno (quadratic 7)', $ ; edgefit, r w/ reproj_sinc=1, delta=30
            'Tiscareno (gaussian 6)' ]   ; edgefit, r w/ reproj_sinc=1, delta=30
!p.charsize = 1.5

if keyword_set(makeplot) then psname = 'compare_spitale_edge'
if keyword_set(dolzr) then begin
  lzr, psname, /half
  @plot_prepare
  plot_color
endif
next:
xtit = 'Sample'
ytit = 'Line (deviation from quadratic fit)'
tit = 'Fit to A Ring Edge in N1493613276'
if keyword_set(makeplot) then begin
  if not keyword_set(num) then begin
    psname = 'compare_spitale_edge'
    num = 0
    !p.multi = [0,4,3]
    !x.margin = 0
    !y.margin = 0
    !x.omargin = [10,3]
    !y.omargin = [4,2]
  endif
  tit = ''
  n1 = ([2,6,2,6,2,2,6,6])[num]
  n2 = ([0,0,1,2,3,4,5,7])[num]
  if num eq 0 or num eq 4 then begin
    ytn = ''
  endif else begin
    ytit = ''
    ytn = replicate(' ',20)
  endelse
  if num eq 4 then begin
    xtn = ''
  endif else if num ge 4 then begin
    xtn = ' '
  endif else begin
    xtit = ''
    xtn = replicate(' ',20)
  endelse
endif else begin
  for j=0,n_elements(filenames)-1 do print, strtrim(j,2)+'     '+filenames[j]
  print, 'Select first file:'
  n1 = 0l
  read, n1
  print, 'Select second file:'
  n2 = 0l
  read, n2
endelse

restore, filenames[n1]
if n1 eq 0 then redge1 = redge_spitale else redge1 = redge_linsam
restore, filenames[n2]
if n2 eq 0 then redge2 = redge_spitale else redge2 = redge_linsam

fit = poly_fit( redge1[1,*], redge1[0,*], 2 )
plot, redge2[1,*], redge2[0,*]-poly(redge2[1,*],fit), /xs, /ys, $
      xtit=xtit, ytit=ytit, tit=tit, xr=[0,1024], yr=[-.3,.3], $
      xtickn=xtn, ytickn=ytn
oplot, !x.crange, [0,0], l=1
oplot, redge1[1,*], redge1[0,*]-poly(redge1[1,*],fit), $
       co=ctred()
xyouts, 50, .2, descrip[n2], chars=1
xyouts, 50, .24, descrip[n1], co=ctred(), chars=1

if keyword_set(makeplot) then begin
  if num lt 7 then begin
    num = num + 1
    goto, next
  endif
  tit = 'Fit to A Ring Edge in N1493613276'
  xyouts, /device, !d.x_size/2, !d.y_size-!d.y_ch_size, align=.5, tit
endif
if keyword_set(dolzr) then clzr

end
