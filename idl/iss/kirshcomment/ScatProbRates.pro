if keyword_set(dolzr) then begin
  lzr, 'ScatProbRates'
  @plot_prepare
endif
!p.multi = [0,2,2]

eh = 1
done = 0
redo:
chi = .0165
dout = 3.5
din = 3.5 - 343./24*chi
dd = findgen(401)/100 + 1
fooin = where( dd le din )
fooout = where( dd le dout )

if keyword_set(period) then begin

  plot, [0,5], [0,45], /xs, /ys, /nodata
  oplot, dd[fooin], 2./3/dd[fooin]/chi - 5./6, l=1
  oplot, dd[fooout], 2./3/dd[fooout]/chi + 5./6, l=1

  fooin1 = where( dd le din-eh )
  fooout1 = where( dd le dout-eh )
  fooin2 = where( dd ge din-eh and dd le din+eh )
  fooout2 = where( dd ge dout-eh and dd le dout+eh )
  oplot, dd[fooin1], 2./3/dd[fooin1]/chi - 5./6
  oplot, dd[fooout1], 2./3/dd[fooout1]/chi + 5./6
  oplot, dd[fooin2], $
         ( 2./3/dd[fooin2]/chi - 5./6 ) / ( 0.5 - (dd[fooin2]-din)/2 )
  oplot, dd[fooout2], $
         ( 2./3/dd[fooout2]/chi + 5./6 ) / ( 0.5 - (dd[fooout2]-dout)/2 )
  
  xyouts, 2, 23, 'Outer'
  xyouts, 2, 16, 'Inner', align=1

endif

plot, [1,5], [0,.11], /xs, /ys, /nodata, $
      xtit='Semimajor Axis (difference from a!Dplanet!N in Hill Radii)', $
      ytit='Synodic Frequency!C(in units of planet'' orbital frequency)'
oplot, dd[fooin], 1/( 2./3/dd[fooin]/chi - 5./6 ), l=1
oplot, dd[fooout], 1/( 2./3/dd[fooout]/chi + 5./6 ), l=1

fooin1 = where( dd le din-eh )
fooout1 = where( dd le dout-eh )
fooin2 = where( dd ge din-eh and dd le din+eh )
fooout2 = where( dd ge dout-eh and dd le dout+eh )
oplot, dd[fooin1], 1/( 2./3/dd[fooin1]/chi - 5./6 )
oplot, dd[fooout1], 1/( 2./3/dd[fooout1]/chi + 5./6 )
oplot, dd[fooin2], $
       ( 0.5 - (dd[fooin2]-din)/2/eh ) / ( 2./3/dd[fooin2]/chi - 5./6 )
oplot, dd[fooout2], $
       ( 0.5 - (dd[fooout2]-dout)/2/eh ) / ( 2./3/dd[fooout2]/chi + 5./6 )

xyouts, 2, .04, 'Outer'
xyouts, 2, .055, 'Inner', align=1
xyouts, 4.7, 0.09, 'e!DH!N = '+string(eh,fo='(F4.2)'), align=1

case eh of
  1: eh = 0.5
  0.5: eh = 0.25
  0.25: eh = 0.1
  0.1: done = 1
endcase
if not keyword_set(done) then goto, redo

if keyword_set(dolzr) then clzr

end
