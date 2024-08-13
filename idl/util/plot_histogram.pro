pro plot_histogram, data, lbins=lbins, lx=lx, np=np, nbins=nbins, lmin=lmin, lmax=lmax, noplot=noplot, xtit=xtit, ytit=ytit, xr=xr, yr=yr, xlog=xlog, ylog=ylog

if not keyword_set(nbins) then nbins=20
if not keyword_set(lmin) then lmin = fix(min(data))
if not keyword_set(lmax) then lmax = ceil(max(data))

lbins = findgen(nbins+1) / nbins * (lmax-lmin) + lmin 
lx = rebin([ [lbins[1:nbins]], [lbins[0:nbins-1]] ],nbins,1) 
np = lonarr(nbins) 

for j=0,nbins-1 do begin 
  foo = where( data ge lbins[j] and $ 
               data lt lbins[j+1], count ) 
  if count gt 0 then np[j] = count 
endfor 

if not keyword_set(noplot) then begin
  plot_nosci, [lbins[0],lx,lbins[nbins]], /xs, ps=10, $ 
              [np[0],np,np[nbins-1]], xlog=xlog, ylog=ylog, $ 
              xtit=xtit, ytit=ytit, xr=xr, yr=yr 
endif

end
