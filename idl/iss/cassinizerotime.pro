; Some times derived from randomly selected images, including earliest
; and (to date) most recent Cassini images:
; IDL> print,caldate((julday(1,9,1999,8,13,58.687)*86400-1294561143)/86400)  
; 1957 DEC 31 23:54:55.687
; IDL> print,caldate((julday(7,1,2004,3,11,40.291)*86400-1467344155)/86400) 
; 1957 DEC 31 23:35:45.291
; IDL> print,caldate((julday(5,1,2005,17,46,30.403)*86400-1493662416)/86400) 
; 1957 DEC 31 23:32:54.403
; IDL> print,caldate((julday(4,22,2008,16,54,9.764)*86400-1587576686)/86400) 
; 1957 DEC 31 23:22:43.764

x = [ julday(1,9,1999,8,13,58.687), julday(7,1,2004,3,11,40.291), $
      julday(5,1,2005,17,46,30.403), julday(4,22,2008,16,54,9.764) ]
y = [ julday(12,31,1957,23,54,55.687), julday(12,31,1957,23,35,45.291), $
      julday(12,31,1957,23,32,54.403), julday(12,31,1957,23,22,43.764) ]
fit = poly_fit( x, y-2451545, 1 )
fit[0] = fit[0] + 2451545  ; Somehow even double-precision wasn't working
plot_nosci, x, y, ps=4, /xs, /ys, xtit='Image Time (Julian days)', $
            ytit='Extrapolated Zero Time (Julian days)', xma=[15,5], $
            xr=[2450300,2455000], yr=[2436204.47d0,2436204.505d0]
oplot, !x.crange, poly( !x.crange, fit ), l=5
oplot, [1,1]*julday(10,15,1997,0,0,0), !y.crange, l=1
xyouts, julday(10,15,1997,0,0,0)-30, mean(!y.crange), 'Cassini Launch', $
        align=.5, orient=90
oplot, [1,1]*julday(1,1,2000,0,0,0), !y.crange, l=1
xyouts, julday(1,1,2000,0,0,0)-30, mean(!y.crange), 'J2000', $
        align=.5, orient=90
oplot, [1,1]*julday(7,1,2004,0,0,0), !y.crange, l=1
xyouts, julday(7,1,2004,0,0,0)-30, mean(!y.crange), 'SOI', $
        align=.5, orient=90
oplot, [1,1]*julday(1,1,2008,0,0,0), !y.crange, l=1
xyouts, julday(1,1,2008,0,0,0)-30, mean(!y.crange), $
        caldate(julday(1,1,2008,0,0,0)), align=.5, orient=90
oplot, !x.crange, [1,1]*julday(1,1,1958,0,0,0), l=1
xyouts, mean(!x.crange), julday(1,1,1958,0,0,0)+.0003, $
        caldate(julday(1,1,1958,0,0,0)), align=.5
oplot, !x.crange, [1,1]*julday(12,31,1957,23,30,0), l=1
xyouts, mean(!x.crange), julday(12,31,1957,23,30,0)+.0003, $
        caldate(julday(12,31,1957,23,30,0)), align=.5

crosspoint = fit[0] / (1-fit[1])
print, 'The lines cross when x = y = '+caldate(crosspoint)

end
