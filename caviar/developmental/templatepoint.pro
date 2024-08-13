PRO templatepoint, filename, temp_radi, temp_val, xest, yest, mnrad=mnrad, mxrad=mxrad, stop=stop, thres=thres, smlen=smlen


if not keyword_set(mnrad) then mnrad=min(temp_radi) 
if not keyword_set(mxrad) then mxrad=max(temp_radi) 

radi=temp_radi
val=temp_val
qr=where(radi ge mnrad and radi le mxrad)
rradi=radi(qr)
vval=val(qr)


im=read_vicar(filename)
par=phasearray(filename, rar=rar, lar=lar, xar=xar, yar=yar)
lar=180/!dpi*atan(yar,xar)
nl=n_elements(im(*,0))
xar=findgen(nl)#(fltarr(nl)+1)
yar=findgen(nl)##(fltarr(nl)+1)
drdx=rar-rar
drdy=rar-rar
drdx(0:nl-2,*)=rar(1:nl-1,*)-rar(0:nl-2,*)
drdy(*,0:nl-2)=rar(*,1:nl-1,*)-rar(*,0:nl-2)

trim=5


qar=where(rar gt mnrad and rar lt mxrad and $
	   xar gt trim and xar lt nl-trim and $
	   yar gt trim and yar lt nl-trim )
imd=im(qar)
rad=rar(qar)
lad=lar(qar)
;xad=xar(qar)
;yad=yar(qar)
drdxd=drdx(qar)
drdyd=drdy(qar)
qs=sort(rad)
rad=rad(qs)
imd=imd(qs)
lad=lad(qs)
;xad=xad(qs)
;yad=yad(qs)
drdxd=drdxd(qs)
drdyd=drdyd(qs)
lad=lad mod 360
lad=(lad+720) mod 360
if max(lad)-min(lad) gt 180 then lad=(lad+180+720) mod 360-180

mxl=max(lad)
mnl=min(lad)
nn=9
dl=(mxl-mnl)/(nn+1)
lonsx=(findgen(nn)+.5)*dl+mnl

nr=n_elements(rradi)
iprofs=fltarr(nn,nr)
xprofs=fltarr(nn,nr)
yprofs=fltarr(nn,nr)
ccors=fltarr(nn,nr/2)
lags=findgen(nr/2)-nr/4
meddr=median(deriv(rad))
drr=median(deriv(rradi))
xfit=fltarr(2,nn)
yfit=fltarr(2,nn)
drfit=fltarr(2,nn)
if not keyword_set(smlen) then smlen=2000.
smfact=smlen/drr
vvalx=vval-smooth(vval,smfact)
xx=findgen(nr)

for i=0,nn-1 do begin &$
	ql=where(lad gt lonsx(i)-dl/2 and lad lt lonsx(i)+dl/2) &$
;	iprofs(i,*)=interpol(smooth(imd(ql),drr/meddr/10),rad(ql), rradi) &$
;	xprofs(i,*)=interpol(smooth(xad(ql), drr/meddr/10),rad(ql), rradi) &$
;	yprofs(i,*)=interpol(smooth(yad(ql), drr/meddr/10),rad(ql), rradi) &$
	iprofs(i,*)=interpol(imd(ql),rad(ql), rradi) &$
	iprofx=iprofs(i,*)-smooth(iprofs(i,*),smfact) &$
	qx=where(rradi gt min(rad(ql)) and rradi lt max(rad(ql)) and xx gt smfact and xx lt nr-smfact) &$
	if n_elements(qx) gt 2*smfact+nr/2  then ccors(i,*)=c_correlate(iprofx(qx),vvalx(qx), lags) &$
;	xprofs(i,*)=interpol(drdxd(ql),rad(ql), rradi) &$
;	yprofs(i,*)=interpol(drdyd(ql),rad(ql), rradi) &$
;	xfit(*,i)=linfit(xprofs(i,*),rradi) &$
;	yfit(*,i)=linfit(yprofs(i,*),rradi) &$
	xfit(1,i)=mean(drdxd(ql)) &$
	yfit(1,i)=mean(drdyd(ql)) &$
	if n_elements(qx) gt 2*smfact+nr/2  then drfit(0,i)=lags(where(ccors(i,*) eq max(ccors(i,*))))*drr &$
	if n_elements(qx) gt 2*smfact+nr/2  then drfit(1,i)=ccors(i,where(ccors(i,*) eq max(ccors(i,*)))) &$
	end



if not keyword_set(thres) then thres=median(drfit(1,*))*.9
qc=where(drfit(1,*) gt thres)
nq=n_elements(qc)
drest=drfit(0,qc)
aest=xfit(1,qc)
best=yfit(1,qc)
coeff=fltarr(2,nq)
coeff(0,*)=aest
coeff(1,*)=best
;results=regress(coeff,reform(drest)) 
results=[0,0]
if n_elements(qc) gt 2 then results=multiple_linear_regression(coeff, reform(drest),1, /no_intercept)
xest=-results(0)
yest=results(1)

if keyword_set(stop) then stop
end
