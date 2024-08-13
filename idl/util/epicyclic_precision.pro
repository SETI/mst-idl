sfile = 'epicyclic_precision.sav'

if keyword_set(epicyclic2) then begin
endif else if keyword_set(findfile(sfile)) then restore, sfile else begin

  ee_max = 0.03d0
  ii_max = 0.03d0
  delta_ee = 0.001d0
  delta_ii = 0.001d0
  delta_ang = !dpi/8
  num_ee = round( ee_max / delta_ee )
  num_ii = round( ii_max / delta_ii )
  num_ang = round( 2*!dpi / delta_ang )
  num = num_ee * num_ii * num_ang^3

  aa = 100000.0d0
  ee_base = (findgen(num_ee)+1)*delta_ee
  ii_base = (findgen(num_ii)+1)*delta_ii
  ang_base = findgen(num_ang)*delta_ang

  epicyclic = dblarr( num, 6 )
  epicyclic[*,0] = aa

  for j=0.,num_ee-1 do $
    epicyclic[num/num_ee*j:num/num_ee*(j+1)-1,1] = ee_base[j]
  for j=0.,num_ee-1 do for k=0.,num_ii-1 do $
    epicyclic[ num/num_ee*(k+j/num_ii) : $
               num/num_ee*(k+(j+1)/num_ii)-1, 2 ] = ii_base[j]

  for j=0,num_ang-1 do for k=0,num_ang-1 do for l=0,num_ang-1 do $
    epicyclic[ lindgen(num/num_ang^3)*num_ang^3 + $
               j + k*num_ang + l*num_ang^2, 3 ] = ang_base[l]
  for j=0,num_ang-1 do for k=0,num_ang-1 do $
    epicyclic[ lindgen(num/num_ang^2)*num_ang^2 + j + k*num_ang, 4 ] = ang_base[k]
  for j=0,num_ang-1 do $
    epicyclic[ lindgen(num/num_ang)*num_ang + j, 5 ] = ang_base[j]

  pos = epicyclic_to_cart( epicyclic, vel=vel, /musat )
  epicyclic2 = cart_to_epicyclic( pos, vel, /musat, tol=tol )

  save, pos, vel, epicyclic, epicyclic2, tol, filename=sfile

endelse

if keyword_set(dolzr) then begin
  lzr, 'epicyclic_precision'
  @plot_prepare
endif
!p.multi = [0,7,3]
!p.charsize = 1.5
!x.margin = 0
!x.omargin = [10,3]
varname = [ 'Semimajor Axis', 'Eccentricity', 'Inclination', $
            'Longitude of Node', 'Argument of Periapse', 'Mean Anomaly', $
            'Longitude of Periapse', 'Mean Longitude' ]
varunit = [ ' (km)', '', replicate(' (!Uo!N)',6) ]
range = [ [0,0], [-.001,.031], [-.001*180/!dpi,.031*180/!dpi], [-15,360], $
          [-15,360], [-15,360], [-15,360], [-15,360] ]
xti = [ 0, 0.01, 0.5, 90, 90, 90, 90, 90 ]
notn = replicate(' ',20)
for j=0,2 do for k=1,7 do begin
  err = epicyclic2[*,j] - epicyclic[*,j]
  if j gt 1 then err = fix_angles( err )*180/!dpi
  case k of
    6: abscissa = epicyclic[*,3] + epicyclic[*,4]
    7: abscissa = epicyclic[*,3] + epicyclic[*,4] + epicyclic[*,5]
    else: abscissa = epicyclic[*,k]
  endcase 
  if k eq 1 then begin
    ytn = ''
    ytit = varname[j]+' Error'+varunit[j]
  endif else begin
    ytn = notn
    ytit = ''
    abscissa = abscissa*180/!dpi
  endelse 
  plot, abscissa, err, xr=range[*,k], /xs, ps=3, xticki=xti[k], $
        ytickn=ytn, xtit=varname[k]+varunit[k], ytit=ytit
endfor
if keyword_set(dolzr) then clzr

end
