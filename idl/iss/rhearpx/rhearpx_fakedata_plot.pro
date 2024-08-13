restore, 'rhea_reproject_grp.sav'
restore, 'rhearpx_fakedata.sav'

sz = size(__imrz_fake)
xs = sz[1]
ys = sz[2]
marg = 30
if keyword_set(paper_plot) then begin
  nx = 2
  ny = 3
  if keyword_set(dolzr) then begin
    lzr, 'rhearpx_fakedata_plot'
    @plot_prepare
    plot_color
    !p.multi = [0,2,5]
  endif else begin
    window, xs=(xs+marg)*nx, ys=(ys+marg)*ny
    tv, replicate( 255, (xs+marg)*nx, (ys+marg)*ny+marg*0.5 )
    !p.multi = [0,2,3]
  endelse
  !p.charsize = 2
endif else begin
  nx = 1
  ny = nfac
  window, xs=(xs+marg)*nx, ys=(ys+marg)*ny+marg*0.5
endelse
for j=1l,nfac-1 do begin
  if keyword_set(paper_plot) then begin
    ; Load color table 15 and fix it
    if !d.name eq 'X' then device, decomposed=0
    loadct, 15
    tvlct, r, g, b, /get
    r[where(r lt g)] = g[where(r lt g)]
    tvlct, r, g, b
    thresh = 5e-6
  endif else thresh = rms[jjj]/2
  notn = replicate(' ',20)
  if j ge 4 then begin
    xtit = 'Distance (R!DR!N)'
    xtn = ''
  endif else begin
    xtit = ''
    xtn = notn
  endelse 
  if j eq 2 or j eq 4 or j eq 6 then begin
    ytit = ''
    ytn = notn
  endif else begin
    ytit = 'Distance (R!DR!N)'
    ytn = ''
  endelse 
  xr = [1.5,6]       ; For image #6, from rhea_reproject_display.pro
  range = 19.706386  ; For image #6, from rhea_reproject_display.pro
  imdisp, __imrz_fake[*,*,j]>(-thresh)<thresh, /axis, xtit=xtit, ytit=ytit, $
          xtickn=xtn, ytickn=ytn, xtickle=1e-10, ytickle=1e-10, $
          xr=xr, yr=[-100,100]*range*60330*6e-6/764, /xs, /ys, $
          pos=[ 0.47*((j+1) mod 2), (6-j)/2*0.16, $
                0.5+0.47*((j+1) mod 2), 0.2+(6-j)/2*0.16 ]
  if _fac[j] mod 1 eq 0 then fo='(I2)' else fo='(F4.1)'
  xyouts, !x.crange[1] - (!x.crange[1]-!x.crange[0])*.02, $
          !y.crange[1] - (!y.crange[1]-!y.crange[0])*.15, $
          string(_fac[j],fo=fo)+' * RMS I/F', /align, /chars
  if j eq 5 then begin
    arrow, 3.5, 0, 2.7, 0, hsize=2*!d.x_size/128, hthick=1, /solid, $
           thick=8, /data
    xyouts, 3.6, -0.15, /data, chars=2, 'Synthetic Ring'
  endif 
endfor
if keyword_set(dolzr) then begin
  tifffile = 'rhearpx_fakedata_plot.tiff'
  if keyword_set(paper_plot) then begin
  endif else begin
    outim = tvrd()
    write_tiff, tifffile, reverse( outim, 2 )
  endelse
endif

if keyword_set(paper_plot) then begin
  scalebar = rebin(rebin( indgen(256), 256, 1 ),256,10)
  imdisp, scalebar, xs=1, /axis, xr=[-thresh,thresh], ytickn=notn, $
          xtickle=1e-10, ytickle=1e-10, xtit='Image Brightness (I/F)', $
          pos=[ 0.52, 0.03, 0.92, 0.13 ]
  if keyword_set(dolzr) then clzr
endif

end

