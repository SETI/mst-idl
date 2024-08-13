function ioverftodn, image_in, lbl, debug=debug, forward=forward

image = image_in
sz = size(image)
if (where(sz[0:2] ne [2,1024,1024]))[0] eq -1 then isimage = 1
CalibrationBaseDir = getenv( 'CalibrationBaseDir' ) + '/'

openr, 1, lbl
aa = ''
while strmid(aa,0,17) ne 'EXPOSURE_DURATION' do readf, 1, aa
start = strpos( aa, ' = ' )
finish = strlen( aa )
ExpDur = strmid( aa, start+3, finish )
close, 1
openr, 1, lbl
aa = ''
while strmid(aa,0,11) ne 'FILTER_NAME' do readf, 1, aa
start = strpos( aa, '(' )
comma = strpos( aa, ',', start )
finish = strpos( aa, ')', comma )
Filter1 = strmid( aa, start+2, comma-start-3 )
Filter2 = strmid( aa, comma+2, finish-comma-3 )
close, 1
openr, 1, lbl
aa = ''
while strmid(aa,0,12) ne 'GAIN_MODE_ID' do readf, 1, aa
start = strpos( aa, ' = "' )
finish = strpos( aa, '"', start+4 )
GainState = strmid( aa, start+4, 2 )
case GainState of
  '12': GainState = 3
  '29': GainState = 2
  '95': GainState = 1
  '215': GainState = 0
endcase 
close, 1
openr, 1, lbl
aa = ''
while strmid(aa,0,10) ne 'IMAGE_TIME' do readf, 1, aa
start = strpos( aa, ' = "' )
finish = strpos( aa, '"', start+4 )
ImgTime = strmid( aa, start+4, finish-start-4 )
close, 1
openr, 1, lbl
aa = ''
while strmid(aa,0,13) ne 'INSTRUMENT_ID' do readf, 1, aa
start = strpos( aa, ' = "' )
finish = strpos( aa, '"', start+4 )
Instrument = strmid( aa, start+4, finish-start-4 )
close, 1
openr, 1, lbl
aa = ''
while strmid(aa,0,18) ne 'OPTICS_TEMPERATURE' do readf, 1, aa
start = strpos( aa, '(' )
comma = strpos( aa, ',', start )
OptT0 = strmid( aa, start+1, comma-start-1 )
close, 1
openr, 1, lbl
aa = ''
while strmid(aa,0,14) ne 'OBJECT = IMAGE' do readf, 1, aa
while strmid(aa,0,11) ne '      LINES' do readf, 1, aa
start = strpos( aa, ' = ' )
finish = strlen( aa )
NL = strmid( aa, start+3, finish )
close, 1
openr, 1, lbl
aa = ''
while strmid(aa,0,18) ne '      LINE_SAMPLES' do readf, 1, aa
start = strpos( aa, ' = ' )
finish = strlen( aa )
NS = strmid( aa, start+3, finish )
close, 1

; In Cisscal, Step 9 of cassimg__radiomcalib.pro is the conversion from DN
; to Flux.  In our case, we always choose the option to report flux as I/F.
; This step has four steps, as follows:
; DNtoElectrons
; The gain (electrons per DN) is nominally 215, 95, 29, or 12, but actually 
; deviates somewhat.  The precise value (TrueGain) depends on NAC or WAC, 
; as well as the gain state used. 
IF Instrument EQ 'ISSNA' THEN BEGIN
    Gain2 = 30.27
    GainRatios = [0.135386, 0.309569, 1.0, 2.357285]
ENDIF ELSE IF Instrument EQ 'ISSWA' THEN BEGIN
    Gain2 = 27.68
    GainRatios = [0.125446, 0.290637, 1.0, 2.360374]
ENDIF
GainState = GainState
TrueGain = Gain2/GainRatios
if keyword_set(forward) then begin
  image = image * TrueGain[GainState]
endif else begin
  image = image / TrueGain[GainState]
endelse

; DivideByExpoT
; The actual exposure time is found in one of a series of files
ShutterOffsetDir = CalibrationBaseDir + 'offset/'
IF Instrument EQ 'ISSNA' THEN FName = 'nacfm_so_' $
	ELSE FName = 'wacfm_so_'
IF OptT0 LT -5.0 THEN Fname = FName + 'm10.img' $
ELSE IF OptT0 LT 25.0 THEN Fname = FName + 'p5.img' $
ELSE FName = FName + 'p25.img'
FName = ShutterOffsetDir + FName
GET_LUN, SoFile
ShutterOffset = read_vicar(Fname)
ShutterOffset = rebin( ShutterOffset, NS )
ExposureTVec = ExpDur - ShutterOffset
ExposureT = TRANSPOSE( INTARR(NL) + 1 ) ## ExposureTVec
ConstOffset = 2.85  
ExposureT = ExposureT - ConstOffset
if keyword_set(forward) then begin
  if keyword_set(isimage) then begin
    image = image * 1000 / ExposureT	; 1000 to scale ms to seconds
  endif else begin
    image = image * 1000 / mean(ExposureT)
  endelse
endif else begin
  if keyword_set(isimage) then begin
    image = image / 1000 * ExposureT	; 1000 to scale ms to seconds
  endif else begin
    image = image / 1000 * mean(ExposureT)
  endelse
endelse

; DivideByAreaPixel
; The OpticsArea depends only on NAC or WAC.  The SumFactor depends on the 
; summation state.  A third parameter, SolidAngle, isn't used in I/F mode. 
IF Instrument EQ 'ISSNA' THEN BEGIN
    SolidAngle = 3.6e-11        ;I get 3.5587e-11 - BDK
    OpticsArea = 264.84
ENDIF ELSE IF Instrument EQ 'ISSWA' THEN BEGIN
    SolidAngle = 3.6E-9         ;I get 3.5587e-9 - BDK
    OpticsArea = 29.32
ENDIF
SumFactor = (NS/1024.0) * (NL/1024.0)
if keyword_set(forward) then begin
  image = image * SumFactor / ( SolidAngle * OpticsArea )
endif else begin
  image = image / SumFactor * ( SolidAngle * OpticsArea )
endelse

; DivideByEfficiency
SatDist = [ 9.18396834, 9.1761802, 9.16855017, 9.16108198, 9.15377802, $
	9.14664156, 9.13967734, 9.13289014, 9.12628487, 9.11986437, $
	9.11363161, 9.10759036, 9.10174584, 9.09610518, 9.09067407, $
	9.08545719, 9.08045717, 9.07567627, 9.07111819, 9.06678709, $
	9.06268822, 9.05882520, 9.05520073, 9.05181736, 9.04867769, $
	9.04578622, 9.04314665, 9.04076177, 9.03863244, 9.03675812, $
	9.03513959, 9.03377846, 9.03267826, 9.03184165, 9.03126961, $
	9.03096216, 9.03091817, 9.03113819, 9.03162294, 9.03237298, $
	9.03338774, 9.03466487, 9.03620286, 9.03800070, 9.04005924, $
	9.04237937, 9.04496010, 9.04779927, 9.05089272, 9.05423742, $
	9.05783135, 9.06167325, 9.06576207, 9.07009469, 9.07466794, $
	9.07947815, 9.08452294, 9.08980088, 9.09530916, 9.10104413, $
	9.10699991, 9.11317104, 9.11955355, 9.12614476, 9.13294309, $
	9.13994497, 9.14714590, 9.15454014, 9.16212231, 9.16988869, $
	9.17783513, 9.18595757, 9.19425017, 9.20270675, 9.21132218, $
	9.22009224, 9.22901443, 9.23808479, 9.24729810, 9.25664757, $
	9.26612574, 9.27572722, 9.28544734, 9.29528251, 9.30522803, $
	9.31527787, 9.32542619, 9.33566724, 9.34599727, 9.35641205, $
	9.36690631 ]
ImgTimeY = STRMID(ImgTime,0,4)
ImgTimeD = STRMID(ImgTime,5,3)
ImgTimeYF = ImgTimeY + (ImgTimeD / 365.0)
dfs = INTERPOLATE(SatDist, (ImgTimeYF-2000)*10)
specfile = CalibrationBaseDir + 'efficiency/solarflux.tab'
ang_to_nm = 10.
pifact = !pi
cisscal_readspec, specfile, lambda_f, flux, n_spec
lambda_f = temporary(lambda_f/ang_to_nm)
flux = temporary(flux*ang_to_nm)
transfile = CalibrationBaseDir + 'efficiency/systrans/' + $
            strlowcase(Instrument + Filter1 + Filter2) + $
            '_systrans.tab'
cisscal_readspec, transfile, lambda_t, trans, n_trans
if Instrument EQ 'ISSNA' then begin
  qecorrfile = CalibrationBaseDir + 'correction/nac_qe_correction.tab'
  cisscal_readspec, qecorrfile, lambda_q, qecorr, n_qecorr
endif else if Instrument EQ 'ISSWA' then begin
  qecorrfile = CalibrationBaseDir + 'correction/wac_qe_correction.tab'
  cisscal_readspec, qecorrfile, lambda_q, qecorr, n_qecorr
endif
minlam=ceil(max([min(lambda_t),min(lambda_f),min(lambda_q)]))
maxlam=floor(min([max(lambda_t),max(lambda_f),max(lambda_q)]))
lambda = [lambda_f,lambda_t,lambda_q]
lambda = lambda[where((lambda ge minlam) and (lambda le maxlam))]
lambda = lambda[uniq(lambda,sort(lambda))]
newtrans = interpol(trans,lambda_t,lambda)
newqecorr = interpol(qecorr,lambda_q,lambda)
newflux = interpol(flux,lambda_f,lambda)/(pifact * dfs^2) ;?
EffFact = int_tabulated(lambda,newtrans*newqecorr*newflux)

if keyword_set(forward) then begin
  image = image / EffFact
endif else begin
  image = image * EffFact
endelse

if keyword_set(debug) then stop

return, image

end
