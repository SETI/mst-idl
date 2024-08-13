function predict_bias, gain, sum, dconv, instrument, $
  noplot=noplot, silent=silent

if not keyword_exists(noplot) then noplot = 1
if not keyword_set(noplot) then begin
  !p.multi = [0,5,5]
  solid_small_circles
endif
savefile = '/home/borogove/matthewt/idl/iss/diffring_exposures/bias.sav'
if not keyword_set(findfile(savefile)) then begin
  basedir = '/home/borogove/iss/images/'
  cd, basedir, current=olddir
  spawn, 'ls', revs
  _bias = 0.0d0
  _gain = ''
  _sum = ''
  _dconv = ''
  _instr = ''
  _filename = ''
  for j=0,n_elements(revs)-1 do begin
    print, strtrim(j,2)+':  '+revs[j]
    if keyword_set(findfile(revs[j]+'/*/*.LBL')) then begin
      spawn, 'grep BIAS_STRIP_MEAN '+revs[j]+'/*/*.LBL', bias
      spawn, 'grep GAIN_MODE_ID '+revs[j]+'/*/*.LBL', gain
      spawn, 'grep INSTRUMENT_MODE_ID '+revs[j]+'/*/*.LBL', sum
      spawn, 'grep DATA_CONVERSION_TYPE '+revs[j]+'/*/*.LBL', dconv
      spawn, 'grep INSTRUMENT_ID '+revs[j]+'/*/*.LBL', instr
      if n_elements(bias) ne n_elements(gain) then stop
      if n_elements(bias) ne n_elements(sum) then stop
      if n_elements(bias) ne n_elements(instr) then stop
      _bias = [ _bias, strmid( bias, rotate(strpos(bias,'='),1)+2, $
                               rotate(strlen(bias),1) ) ]
      _gain = [ _gain, strmid( gain, rotate(strpos(gain,'='),1)+3, 3 ) ]
      _sum = [ _sum, strmid( sum, rotate(strpos(sum,'='),1)+2, $
                             rotate(strlen(sum),1) ) ]
      _dconv = [ _dconv, strmid( dconv, rotate(strpos(dconv,'='),1)+2, $
                                 rotate(strlen(dconv),1) ) ]
      _instr = [ _instr, strmid( instr, rotate(strpos(instr,'='),1)+3, 5 ) ]
      _filename = [ _filename, strmid( sum, 0, rotate(strpos(sum,'.LBL'),1) ) ]
    endif
  endfor
  _bias = clip(_bias)
  _gain = clip(_gain)
  _sum = clip(_sum)
  _dconv = clip(_dconv)
  _instr = clip(_instr)
  _filename = clip(_filename)
  save, _bias, _gain, _sum, _dconv, _instr, _filename, filename=savefile
endif else restore, savefile

if not ( keyword_set(biaserr) and keyword_set(noplot) ) then begin
  gainstates = [ '215', '95 ', '29 ', '12 ' ]
  sumstates = [ '"FULL"', '"SUM2"', '"SUM4"' ]
  dconvstates = [ '"12BIT"', '"8LSB"', '"TABLE"' ]
  instruments = [ 'ISSNA', 'ISSWA' ]
  bias = fltarr( 4, 3, 3, 2 )
  biasmean = fltarr( 4, 3, 3, 2 )
  biaserr = fltarr( 4, 3, 3, 2 )
  for instri=0,1 do for sumi=0,2 do for dconvi=0,2 do for gaini=0,3 do begin
    foo = where( _gain eq gainstates[gaini] and $
                 _sum eq sumstates[sumi] and $
                 _dconv eq dconvstates[dconvi] and $
                 _instr eq instruments[instri], count )
    if count gt 0 then begin
      if count eq 1 then begin
        bias[gaini,sumi,dconvi,instri] = _bias[foo]
        biasmean[gaini,sumi,dconvi,instri] = _bias[foo]
      endif else begin
        bias[gaini,sumi,dconvi,instri] = max( _bias[foo] )
        biasmean[gaini,sumi,dconvi,instri] = mean( _bias[foo] )
        biaserr[gaini,sumi,dconvi,instri] = stddev( _bias[foo] )
        if not keyword_set(noplot) then begin
          plot, _bias[foo], ps=8, /xs, /ys, xtit='Images', ytit='Bias (DN)', $
                tit=instruments[instri]+', '+sumstates[sumi]+', '+$
                dconvstates[dconvi]+', gain='+gainstates[gaini], $
                xma=[9,2], yma=[4,1]
;          stop
        endif
      endelse
    endif
  endfor
  save, bias, biasmean, biaserr, gainstates, sumstates, dconvstates, $
        instruments, $
        _bias, _gain, _sum, _dconv, _instr, _filename, filename=savefile
  if not keyword_set(olddir) then olddir = '.'
  cd, olddir
  gain = 0
  gaini = 3
  sum = 0
  sumi = 1
endif

if not keyword_set(gain) then gaini = 3 else begin
  if (size(gain))[(size(gain))[0]+1] eq 7 then begin
    foo = where( gain eq gainstates, count )
    if count gt 0 then gaini = foo[0]
  endif else begin
    if gain eq 215 then gaini = 0
    if gain eq 95 then gaini = 1
    if gain eq 29 then gaini = 2
    if gain eq 12 then gaini = 3
  endelse
  if not keyword_set(gaini) then gaini = 3
endelse
if not keyword_set(sum) then sumi = 1 else begin
  if (size(sum))[(size(sum))[0]+1] eq 7 then begin
    foo = where( sum eq sumstates, count )
    if count gt 0 then sumi = foo[0]
  endif else begin
    if sum eq 1 then sumi = 0
    if sum eq 2 then sumi = 1
    if sum eq 4 then sumi = 2
  endelse
  if not keyword_set(sumi) then sumi = 1
endelse
if not keyword_set(dconv) then dconvi = 0 else begin
  if dconv eq '12BIT' then dconvi = 0
  if dconv eq '8LSB' then dconvi = 1
  if dconv eq 'TABLE' then dconvi = 2
  foo = where( dconv eq dconvstates, count )
  if count gt 0 then dconvi = foo[0]
  if not keyword_set(dconvi) then dconvi = 0
endelse
if not keyword_set(instrument) then instri = 0 else begin
  if instrument eq 'WAC' then instri = 1 else instri = 0
  foo = where( instrument eq instruments, count )
  if count gt 0 then instri = foo[0]
endelse

out = bias[ gaini, sumi, dconvi, instri ]
if not keyword_set(silent) then begin
  print, instruments[instri]+', '+$
         sumstates[sumi]+', '+dconvstates[dconvi]+', gain='+$
         gainstates[gaini]
  print, out
endif
return, out

end
