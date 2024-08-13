dirs = [ '046/RDHRESSCN', '046/RDHRESSCN', '046/RDHRESSCN', $
         '116/EQXSHADOW001', '116/EQXSHADOW001' ]
images = [ 'N1560311316_1_cal.IMG', 'N1560311433_1_cal.IMG', $
           'N1560311549_1_cal.IMG', $
           'N1628594653_1_cal.IMG', 'N1628594713_1_cal.IMG' ]
orbit = strmid( dirs, 0, 3 )
ndirs = n_elements(dirs)
nimages = n_elements(images)
ddirs = dirs[ uniq(images) ]
for k=0,ndirs-1 do begin
  restore, '/home/borogove/iss/images/'+dirs[k]+'/stretch.sav'
  restore, '/home/borogove/iss/images/'+dirs[k]+'/spreadsheet.sav'
  restore, '/home/borogove/iss/images/'+dirs[k]+'/radial_scale_adj.sav'
  foo = where( ddirs eq dirs[k], count )
  if keyword_set(condense) then begin
    jjj = (where( filenames eq images[foo[0]], count1 ))[0]
    if count1 ne 1 then stop
    for j=1,count-1 do begin
      jjj = [ jjj, (where( filenames eq images[foo[j]], count1 ))[0] ]
      if count1 ne 1 then stop
    endfor
    text = strmid(dirs[k],0,3) + ' & ' + strmid(filenames[jjj[0]],0,11)
    if count gt 1 then begin
      text = text + ' -- ' + strmid(filenames[jjj[count-1]],6,5)
    endif 
    text = text + ' & ' + strtrim(count,2) + ' & ' + strmid(data[2,0],0,4) + $
           '-' + strmid(data[2,0],4,3)
    if strmid(data[2,count-1],4,3) ne strmid(data[2,0],4,3) then begin
      text = text + '/' + strmid(data[2,count-1],4,3)
    endif 
    text = text + ' & ' + $
           string( mean(_keywords[jjj].ringplane_aimpoint_incidence_angle), $
                       fo='(F5.1)' ) + '$^\circ$'
    var = max(_keywords[jjj].ringplane_aimpoint_emission_angle) - $
          min(_keywords[jjj].ringplane_aimpoint_emission_angle)
    if var gt 9 then begin
      text = text + ' & ' + $
             string( min(_keywords[jjj].ringplane_aimpoint_emission_angle), $
                     fo='(I3)' ) + '$^\circ$ -- ' + $
             string( max(_keywords[jjj].ringplane_aimpoint_emission_angle), $
                     fo='(I3)' ) + '$^\circ$'
    endif else begin
      text = text + ' & ' + $
             string( mean(_keywords[jjj].ringplane_aimpoint_emission_angle), $
                     fo='(F5.1)' ) + '$^\circ$ '
    endelse 
    var = max(_keywords[jjj].ringplane_aimpoint_phase_angle) - $
          min(_keywords[jjj].ringplane_aimpoint_phase_angle)
    if var gt 9 then begin
      text = text + ' & ' + $
             string( min(_keywords[jjj].ringplane_aimpoint_phase_angle), $
                     fo='(I3)' ) + '$^\circ$ -- ' + $
             string( max(_keywords[jjj].ringplane_aimpoint_phase_angle), $
                     fo='(I3)' ) + '$^\circ$'
    endif else begin
      text = text + ' & ' + $
             string( mean(_keywords[jjj].ringplane_aimpoint_phase_angle), $
                     fo='(F5.1)' ) + '$^\circ$ '
    endelse 
    var = max(_keywords[jjj].ringplane_aimpoint_radial_scale) - $
          min(_keywords[jjj].ringplane_aimpoint_radial_scale)
    if var gt 1 then begin
      if var gt 2 then fo='(I3)' else fo='(F5.1)'
      text = text + ' & ' + $
             string( min(_keywords[jjj].ringplane_aimpoint_radial_scale), $
                     fo=fo ) + ' -- ' + $
             string( max(_keywords[jjj].ringplane_aimpoint_radial_scale), $
                     fo=fo )
    endif else begin
      text = text + ' & ' + $
             string( mean(_keywords[jjj].ringplane_aimpoint_radial_scale), $
                     fo='(F5.1)' )
    endelse 
    var = max(_keywords[jjj].ringplane_aimpoint_longitudinal_scale_km) - $
          min(_keywords[jjj].ringplane_aimpoint_longitudinal_scale_km)
    if var gt 1 then begin
      if var gt 2 then fo='(I3)' else fo='(F5.1)'
      text = text + ' & ' + $
             string( min($
                  _keywords[jjj].ringplane_aimpoint_longitudinal_scale_km), $
                     fo=fo ) + ' -- ' + $
             string( max($
                  _keywords[jjj].ringplane_aimpoint_longitudinal_scale_km), $
                     fo=fo )
    endif else begin
    text = text + ' & ' + $
           string( mean($
               _keywords[jjj].ringplane_aimpoint_longitudinal_scale_km), $
                   fo='(F5.1)' )
    endelse 
    if ( mean(_keywords[jjj].ringplane_aimpoint_incidence_angle) gt 90 ) xor $
       ( mean(_keywords[jjj].ringplane_aimpoint_emission_angle) gt 90 ) then $
          begin
      text = text + ' & Unlit'
    endif else begin
      text = text + ' & Lit'
;      dirs_lit[k] = 1
    endelse 
    text = text + ' \\'
    print, text
  endif else begin
    for j=0,count-1 do begin
      jjj = (where( data[0,*] eq strmid(images[foo[j]],0,13), count1 ))[0]
      if count1 ne 1 then stop
      text = strmid(images[foo[j]],0,11) + ' & ' + orbit[k] + ' & '
      if keyword_set(indnum) then text = text + string(jjj,fo='(I03)') + ' & '
      text = text + strmid(data[2,jjj],0,4) + '-' + strmid(data[2,jjj],4,3) + $
             ' ' + strmid(data[2,jjj],7,2) + ':' + strmid(data[2,jjj],9,2) + $
             ':' + strmid(data[2,jjj],11,2) + ' & '
      jjj = (where( filenames eq images[foo[j]], count1 ))[0]
      if count1 ne 1 then stop
      text = text + $
             string( _keywords[jjj].ringplane_aimpoint_incidence_angle, $
                     fo='(F6.2)' ) + '$^\circ$ & ' + $
             string( _keywords[jjj].ringplane_aimpoint_emission_angle, $
                     fo='(F5.1)' ) + '$^\circ$ & ' + $
             string( _keywords[jjj].ringplane_aimpoint_phase_angle, $
                     fo='(F5.1)' ) + '$^\circ$ & ' + $
;             string( _keywords[jjj].ringplane_aimpoint_radial_scale, $
;                     fo='(F5.1)' ) + ' & ' + $
             string( abs(_radial_scale_adj[(where(_jjj eq jjj))[0]]), $
                     fo='(F5.1)' ) + ' & ' + $
             string( _keywords[jjj].ringplane_aimpoint_longitudinal_scale_km, $
                     fo='(F6.1)' ) + ' & ' + $
             string( exposure_duration[jjj]/1000., $
                     fo='(F5.1)' )
      case strmid(text,0,11) of
        'N1493791286':  text = text + ' & C1, C2 \\'
        'N1493791602':  text = text + ' & C3, C4 \\'
        'N1493792122':  text = text + ' & C5 \\'
        'N1628592706':  text = text + ' & B \\'
        'N1628669191':  text = text + ' & C \\'
        'N1628845283':  text = text + ' & A1 \\'
        'N1628933401':  text = text + ' & A2 \\'
        'N1721654859':  text = text + ' & C6 \\'
        'N1721654965':  text = text + ' & C6 \\'
        else:  text = text + ' \\'
      endcase 
      print, text
    endfor
    print, '\hline  % '+dirs[k]
  endelse

endfor

end
