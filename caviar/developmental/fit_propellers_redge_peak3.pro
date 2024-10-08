; For Rev 116: 
; restore, 'fit_propellers_redge*'
; impact = 1
; .run fit_propellers_redge1
if !d.name eq 'X' then window, 12

image = images[j]
if not keyword_exists(dotau) then dotau = 1
spawn, 'pwd', pwd
pwd = pwd[0]
secondtolastslash = rstrpos( strmid(pwd,0,rstrpos(pwd,'/')), '/' )
pwd = strmid( pwd, secondtolastslash+1, 1000 )
if pwd eq '007/HIPHASE' then begin
  restore, 'prop_reproj_redge.sav'
  restore, 'fit_propellers_redge1.sav'
  npr = n_elements(prop_redge)
  j = 0
  donext: 
  sz = size(*(prop_reproj[j].rrpi))
  mnrad = prop_reproj[j].radlon[0]
  mxrad = prop_reproj[j].radlon[1]
  mnlon = prop_reproj[j].radlon[2]
  mxlon = prop_reproj[j].radlon[3]
  radx = make_radi( mnrad, mxrad, mnlon, mxlon, sz, loni=lonx, $
                    dradi=dradx, dloni=dlonx )
  redge = *(prop_redge[j].redge)
  redge_sigma = *(prop_redge[j].redge_sigma)
  redge = redge[*,edgebnd[0,j]:edgebnd[1,j]]
  redge_sigma = redge_sigma[*,edgebnd[0,j]:edgebnd[1,j]]
  good1 = indgen(edgebnd[1,j]-edgebnd[0,j]+1); + edgebnd[0,j]
  ng1 = n_elements(good1)
endif

if keyword_set(dotau) then begin
  unit = '!Mt'
  mu = 1
  pomega0 = 0.5
  phasefunc = 1
  case pwd of
    '116/SATELLORB': begin ;A1
      phaseang = 90.8
      ;phasefunc = 0.2
      ;print, 'phasefunc value is for q=4.  Multiply by 2 for q=5, or divide by 3 for q=3.'
      phasefunc = 0.2/3
      print, 'phasefunc value is for q=3.  Multiply by 3 for q=4, or by 6 for q=5.'
    end 
    '116/EQXSHADOW013': begin ;A2
      phaseang = 100.3
      ;phasefunc = 0.3
      ;print, 'phasefunc value is for q=4.  Multiply by 2 for q=5, or divide by 3 for q=3.'
      phasefunc = 0.3/3
      print, 'phasefunc value is for q=3.  Multiply by 3 for q=4, or by 6 for q=5.'
    end 
    '116/EQXSHADOW005': begin ;C
      phaseang = 149.3
      phasefunc = 3.0
    end 
    '116/EQXSHADOW001': begin ;B
      phaseang = 150.6
      phasefunc = 3.0
    end 
    '007/HIPHASE': begin ;C1 thru C5 (non-equinox)
      phaseang = ([174.67,173.97,172.76])[prop_reproj[j].images]
      phasefunc = 20.0
    end 
  endcase 
  const = 4*mu/pomega0/phasefunc
  peakheight = const * redge[2,*]
  peakheight_sigma = const * redge_sigma[2,*]
endif else begin
  unit = 'I/F'
  peakheight = redge[2,*]
  peakheight_sigma = redge_sigma[2,*]
endelse

peakvol = sqrt(2*!pi) * peakheight * redge[4,*]*dradx > 0
peakvol_sigma = sqrt(2*!pi)*dradx * $
                sqrt( peakheight^2*redge_sigma[4,*]^2 + $
                      peakheight_sigma^2*redge[4,*]^2 )

!p.multi = [0,1,3]
!p.charsize = 2
yr1 = [ min(redge[1,good1]), max(redge[1,good1]) ]
yr1 = yr1 + [-0.1,0.1]*(yr1[1]-yr1[0])
plot_nosci, redge[ 0, good1[[0,ng1-1]] ], yr1, /nodata, /xs, /ys, $
            ytit='Radius (km)', xtickn=notn
polyfill, [ reform(redge[0,good1]), reverse(reform(redge[0,good1])) ], $
          [ reform(redge[1,good1]-redge_sigma[1,good1]), $
            reverse(reform(redge[1,good1]+redge_sigma[1,good1])) ], $
          noclip=0, color=ctgray()
oplot, redge[0,good1], redge[1,good1]
yr2 = [ min(peakheight[good1]), max(peakheight[good1]) ]
yr2 = yr2 + [-0.1,0.1]*(yr2[1]-yr2[0])
plot_nosci, redge[ 0, good1[[0,ng1-1]] ], yr2, /nodata, /xs, /ys, $
            ytit='Peak Depth ('+unit+')', xtickn=notn
polyfill, [ reform(redge[0,good1]), reverse(reform(redge[0,good1])) ], $
          [ reform(peakheight[good1]-peakheight_sigma[good1]), $
            reverse(reform(peakheight[good1]+peakheight_sigma[good1])) ], $
          noclip=0, color=ctgray()
oplot, redge[0,good1], peakheight[good1]

yr3 = [ min(peakvol[good1]), max(peakvol[good1]) ]
yr3 = yr3 + [-0.1,0.1]*(yr3[1]-yr3[0])
plot_nosci, redge[ 0, good1[[0,ng1-1]] ], yr3, /nodata, /xs, /ys, $
            ytit='Peak Volume ('+unit+'*km)', xtit='Longitude (!Uo!N)'
polyfill, [ reform(redge[0,good1]), reverse(reform(redge[0,good1])) ], $
          [ reform(peakvol[good1]-peakvol_sigma[good1]), $
            reverse(reform(peakvol[good1]+peakvol_sigma[good1])) ], $
          noclip=0, color=ctgray()
oplot, redge[0,good1], peakvol[good1]

dlonr = dlonx * !pi/180 * mean(redge[1,good1])
ea = total( peakvol[good1] )*dlonr
ea_sigma = sqrt(total( peakvol[good1]^2 ))*dlonr
peak_ioverf = max(redge[2,good1])
if keyword_set(dotau) then peak_tau = max(peakheight)

print, 'Peak I/F:  '+strtrim(peak_ioverf,2)
if keyword_set(dotau) then print, 'Peak tau:  '+strtrim(peak_tau,2)
print, 'Equivalent Area ('+unit+'*km^2):  '+strtrim(ea,2)+' +- '+strtrim(ea_sigma,2)

if pwd eq '007/HIPHASE' then begin
  if j eq 0 then begin
    _ea = ea
    _ea_sigma = ea_sigma
    _peak_ioverf = peak_ioverf
    _peak_tau = peak_tau
    _const = const
  endif else begin
    _ea = [ _ea, ea ]
    _ea_sigma = [ _ea_sigma, ea_sigma ]
    _peak_ioverf = [ _peak_ioverf, peak_ioverf ]
    _peak_tau = [ _peak_tau, peak_tau ]
    _const = [ _const, const ]
  endelse
  if j eq 4 then begin
    ea = _ea
    ea_sigma = _ea_sigma
    peak_ioverf = _peak_ioverf
    peak_tau = _peak_tau
    const = _const
  endif else begin
    stop
    j = j + 1
    goto, donext
  endelse
endif

sfile='fit_propellers_redge_peak3.sav'
if keyword_set(newsave) or not keyword_set(findfile(sfile)) then begin
  if keyword_set(dotau) then begin
    save, image, ea, ea_sigma, peak_ioverf, peak_tau, const, filename=sfile
  endif
endif

end
