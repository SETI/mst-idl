pro plot_blank, noerase=noerase, tit=tit, xma=xma, yma=yma

;plot, indgen(20)/19, indgen(20)/19, co=256, noerase=noerase, $
;	xtickn=replicate(' ',20), ytickn=replicate(' ',20)
if not keyword_set(tit) then tit=''
plot, [0,1], [0,1], /nodata, xs=4, ys=4, tit=tit, noerase=noerase, $
      xma=xma, yma=yma

end
