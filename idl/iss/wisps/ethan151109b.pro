restore, 'image_coverage.sav'
radcoord = where( gridrad eq 136520, count )
if count ne 1 then stop
nbin = 360
xbin = findgen(nbin+1)/nbin*360
xind = rebin([[ xbin[0:nbin-1] ],[ xbin[1:nbin] ]],nbin,1)
note = 'This is the longitudinal coverage at r = 136520 km, for purposes of tracking wisps in the Keeler Gap outer edge.'
coverage_wisp = rebin( [grid1[*,radcoord],0], nbin )

save, note, coverage_wisp, xbin, xind, filename='image_coverage_wisp.sav'

plot, indgen(nbin/10)*10+5, rebin(coverage_wisp,nbin/10), ps=4, xr=[0,360], /xs

end
