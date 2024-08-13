function naifsat, satname, debug=debug

; For input string satname, returns the NAIF code (Saturnian satellites only)

satnames = [ 'Mimas', 'Enceladus', 'Tethys', 'Dione', 'Rhea', 'Titan', $
             'Hyperion', 'Iapetus', 'Phoebe', 'Janus', 'Epimetheus', $
             'Helene', 'Telesto', 'Calypso', 'Atlas', 'Prometheus', $
             'Pandora', 'Pan', 'Ymir', 'Paaliaq', 'Tarvos', 'Ijiraq', $
             'Suttungr', 'Kiviuq', 'Mundilfari', 'Albiorix', 'Skathi', $
             'Erriapo', 'Siarnaq', 'Thrymr', 'Narvi', 'Methone', 'Pallene', $
             'Polydeuces', 'Daphnis', 'Aegir', 'Bebhionn', 'Bergelmir', $
             'Bestla', 'Farbauti', 'Fenrir', 'Fornjot', 'Hati', 'Hyrrokkin', $
             'Kari', 'Loge', 'Skoll', 'Surtur', 'Anthe', 'Jarnsaxa', 'Greip', $
             'Tarqeq', 'Aegaeon' ]

sz = size(satname)
if sz[sz[0]+1] ne 7 then begin

  _satname = satname - 601
  if keyword_set(debug) then stop
  return, satnames[_satname]

endif else begin

  loc = where( satname eq satnames, count )

  if count eq 0 then begin
    satnames = [ 'Mi', 'En', 'Te', 'Di', 'Rh', 'Ti', 'Hy', 'Ia', 'Ph', $
                 'Ja', 'Ep', 'He', 'Tl', 'Ca', 'At', 'Pr', 'Pd', 'Pan', $
                 replicate('',13), 'Me', 'Pal', 'Pol', 'Da', replicate('',13), $
                 'An', replicate('',3), 'Ae' ]
    loc = where( satname eq satnames, count )
    if count eq 0 then begin
      print, 'Input satellite name not found:  '+satname
      return, ''
    endif
  endif

  if loc[0] eq 48 then begin
    print, 'NOTE: In some provisional kernels, Anthe is designated 65030.'
  endif 
  if keyword_set(debug) then stop
  return, long( 601 + loc[0] )

endelse

end
