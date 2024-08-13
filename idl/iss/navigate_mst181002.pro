cspice_furnsh, '/Users/matthewt/Data/NAIF/rfrench/06295_06328pg_fsiv-nav.bc'
cspice_furnsh, '/Users/matthewt/Data/NAIF/sclk/cas00172.tsc'
cspice_furnsh, '/Users/matthewt/Data/NAIF/lsk/naif0012.tls' 
sc=-82L
nj = 26
if keyword_set(n1540682398) then det = 5
if keyword_set(n1540682398) then dt = 1
if keyword_set(n1540682398) then psname = 'navigate_mst181002_N1540682398'
if keyword_set(n1540681817) then det = 3.3
if keyword_set(n1540681817) then dt = .05
if keyword_set(n1540681817) then psname = 'navigate_mst181002_N1540681817'
_pmat = dblarr(3,3,nj)
_ctime = strarr(nj)
for j=0,nj-1 do begin
  if keyword_set(n1540682398) then begin
    sclk = '1540682398.118'
    exposure = '15000.000000'
  endif
  if keyword_set(n1540681817) then begin
    sclk = '1540681817.118'
    exposure = '460.000000'
  endif
  exposure = double(exposure)/1000.0d0
  cspice_scencd,sc,sclk,sclkdp
  cspice_sct2e,sc,sclkdp,et 
  ;et=et-(exposure*0.5d0)
  et=et-exposure+(j-det)*dt
  cspice_sce2t, sc, et, sclkdp
  cspice_ckgp,-82000L,sclkdp,1000.0d0,'J2000',pmat,clkout,found
  cspice_timout,et,'YYYY-DOYTHR:MN:SC.###::UTC',21,ctime
  _pmat[*,*,j] = pmat
  _ctime[j] = ctime
endfor

if keyword_set(dolzr) then begin
  lzr, psname
  @plot_prepare
endif

sec = strmid(_ctime,15,5) 
!p.multi = [0,3,3]
!y.margin = 0
!y.omargin = [4,2]
!p.charsize = 2
solid_circles
for k=0,2 do for j=0,2 do begin
  if k eq 2 then xtn = 0 else xtn = replicate(' ',20)
  if k eq 2 then xtit = strmid(_ctime[0],0,15) else xtit = ''
  if k eq 0 and j eq 1 then begin
    tit = 'N'+strmid(sclk,0,10)+' (031/RDHRCOMP), '+$
          strtrim(fix(exposure*1000),2)+'-millisecond exposure'
  endif else tit=''
  plot_nosci, sec, _pmat[j,k,*], /xs, /ys, ps=-8, xtickn=xtn, tit=tit, $
              xtit=xtit, ytit = 'pmat['+strtrim(j,2)+','+strtrim(k,2)+']'
  oplot, mean(!x.crange)+exposure*[-.5,-.5], !y.crange, l=1
  oplot, mean(!x.crange)+exposure*[+.5,+.5], !y.crange, l=1
endfor

if keyword_set(dolzr) then clzr

end

; Notes: The call to cspice_ckgp is the one that queries the BC kernel
