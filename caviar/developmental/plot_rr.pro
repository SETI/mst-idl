restore, 'ring_rads.sav.2'
o = order
rr = _ring_rads
rrl = _ring_rads_legend
restore, 'ring_rads.sav'

rrl[43:44]=rrl[[44,43]]
rr[43:44]=rr[[44,43]]
rrl[264:265]=rrl[[265,264]]
rr[264:265]=rr[[265,264]]
foo = where( rrl ne _ring_rads_legend, count )
if count gt 0 then stop

foo1 = where( rr ne _ring_rads )
foo2 = where( abs(rr-_ring_rads) gt .8, n2 )
plot, rr[foo1]/1e3, abs(_ring_rads[foo1]-rr[foo1]), /xs, /ys, ps=4, /ylog, $
      xr=[60,145], yr=[.002,600], xtit='Radius (kkm)', xma=[12,3], $
      ytit='Difference between two methods of!Ccalculating resonance locations (km)'
xyouts, rr[foo2[indgen(n2/2)*2]]/1e3, $
        abs(_ring_rads[foo2[indgen(n2/2)*2]]-rr[foo2[indgen(n2/2)*2]]), $
        '  '+rrl[foo2[indgen(n2/2)*2]]+'  ', orient=60
xyouts, rr[foo2[indgen(n2/2)*2+1]]/1e3, $
        abs(_ring_rads[foo2[indgen(n2/2)*2+1]]-rr[foo2[indgen(n2/2)*2+1]]), $
        '  '+rrl[foo2[indgen(n2/2)*2+1]]+'  ', align=1, orient=60
oplot, rr[foo1]/1e3, abs(_ring_rads[foo1]-rr[foo1]), ps=4, co=ctred()

oplot, [74.658,74.658,91.975,91.975,117.507,117.507,122.34,122.34,136.78,136.78], 1e-10+1e10*[1,0,0,1,1,0,0,1,1,0], l=1

end
