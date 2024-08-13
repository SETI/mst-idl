function h_root, r0
  common h1, h
  return, h - r0^2*sqrt(caviar_omega2(r0))
end

function ex_root, x
  common h2, v, e, f
  return, [ e*cos(f) - v - (1+v)*(-x[0]^2+x[0]*cos(x[1])+x[0]^2*cos(2*x[1])), $
            e*sin(f) - (1+v)*(x[0]*sin(x[1])+x[0]^2*sin(2*x[1])) ]
end

function elems_to_epicyclic, elems, true_anom, check=check, debug=debug, verbose=verbose, convert=convert, mu=gm, prad=prad, musat=musat

if n_params() eq 0 then begin
  print, 'Syntax:  Result = ELEMS_TO_EPICYCLIC( elems, true_anom )'
  print, 'From instantaneous osculating orbital elements, calculates '
  print, 'epicyclic elements introduced by Borderies and Longaretti (1987). '
endif

stop, 'This routine, ELEMS_TO_EPICYCLIC, is deprecated in favor of the new CART_TO_EPICYCLIC, based on Renner and Sicardy (2006)'

; Get Saturn mass and radius
if keyword_set(musat) then saturn_constants, gm=gm, prad=prad
if not keyword_set(gm) then stop, 'Must specify central mass mu'
if not keyword_set(prad) then stop, 'Must specify central body radius prad'

if keyword_set(convert) then convm = 'Converting...' else convm = 'Convert?'
; Check that semimajor axis is in km
if mean(elems[*,0]) lt prad then begin
  print, 'elems appears to be in Rsat instead of km.  '+convm
  if not keyword_set(convert) then stop
  elems[*,0] = elems[*,0] * prad
endif

; Check that angles are in radians
if max(elems[*,2:5])-min(elems[*,2:5]) gt 6.3 then begin
  print, 'elems appears to be in degrees instead of radians.  '+convm
  if not keyword_set(convert) then stop
  elems[*,2:5] = elems[*,2:5] * !dpi / 180
endif

if max(true_anom)-min(true_anom) gt 6.3 then begin
  print, 'true_anom appears to be in degrees instead of radians.  '+convm
  if not keyword_set(convert) then stop
  true_anom = true_anom * !dpi / 180
endif

; Check for vectors with all elements set to zero; skip these
nn = (size(elems))[1]
skip = v_mag(elems) eq 0
foo = where( skip, countskip )
if countskip eq nn then return, elems*0

; Calculate angular momentum from osculating elements, use it to get r0
; From Longaretti and Borderies (1991) Eqs 33 and 35.
if not keyword_exists(verbose) and nn gt 10000 then verbose = 1
if n_elements(true_anom) ne nn then stop, 'true_anom must have same dimensions'
r0 = dblarr(nn)
if keyword_set(verbose) then print, 'Finding r0...'
common h1, h
for j=0l,nn-1 do begin
  if j ne 0 and j mod 100000 eq 0 then print, 'j = '+strtrim(j,2)+' / '+strtrim(nn,2)
  if skip[j] eq 1 then r0[j] = 0 else begin
    r00 = elems[j,0] + [ -5, 0, 5 ]
    h = sqrt( gm*elems[j,0]*(1-elems[j,1]^2) )
    if finite(h) then begin
      r0[j] = fx_root( r00, 'h_root', /double )
    endif else begin
      r0[j] = !values.d_nan
    endelse
  endelse
endfor

; BL87 Eq.47
omega2=caviar_omega2(r0)
vv = omega2 * r0^3 / gm - 1

if keyword_set(verbose) then print, 'Finding epsilon and xi...'
; Use BL87 Eqs 53 and 55 to get epsilon and xi
epsilon = dblarr(nn)
xi = dblarr(nn)
common h2, v, e, f
for j=0l,nn-1 do begin
  if j ne 0 and j mod 100000 eq 0 then print, 'j = '+strtrim(j,2)+' / '+strtrim(nn,2)
  if skip[j] eq 1 then begin
    epsilon[j] = 0
    xi[j] = 0
  endif else begin
    v = vv[j]
    e = elems[j,1]
    f = true_anom[j]
    x0 = [ e, f ]
    if finite(v) then begin
      x = newton( x0, 'ex_root', /double )
      if x[0] ge 0 then begin
        epsilon[j] = x[0]
        xi[j] = fix_angles( x[1], /rad, /to360 )
      endif else begin
        epsilon[j] = -x[0]
        xi[j] = fix_angles( x[1] + !dpi, /rad, /to360 )
      endelse
    endif else begin
      epsilon[j] = !values.d_nan
      xi[j] = !values.d_nan
    endelse
  endelse
endfor

; Use BL87 Eq 35 to get theta00, the geometric longitude of pericenter
lon = total(elems[*,3:4],2) + true_anom
theta00 = fix_angles( lon - xi - 2*epsilon*sin(xi), /rad, /to360 )
if countskip gt 0 then theta00[where(skip)] = 0

if keyword_set(check) then begin

  !p.multi = [0,2,4]
  if not keyword_set(!p.charsize) then !p.charsize = 1.5
  oldym = !y.margin
  oldyom = !y.omargin
  !y.margin = 0
  !y.omargin = [4,2]
  notn = replicate(' ',20)

  ; Reconstruct osculating a and e from epicyclic elements
  ; BL87 Eq 48
  plot_nosci, elems[*,0], /xs, /ys, xtickn=notn, ytit='a'
  oplot, r0 / ( (1-vv)*(1-epsilon^2) - 2*vv*epsilon*cos(xi) ), $
         co=ctorange()
  plot_nosci, /xs, /ys, xtickn=notn, ytit='a residual', elems[*,0] - $
              r0 / ( (1-vv)*(1-epsilon^2) - 2*vv*epsilon*cos(xi) )
  ; BL87 Eq 53
  plot_nosci, elems[*,1], /xs, /ys, xtickn=notn, ytit='e'
  oplot, sqrt( vv^2 + (1-vv^2)*epsilon^2 + $
                          2*vv*(1+vv)*epsilon*cos(xi) ), co=ctorange()
  plot_nosci, /xs, /ys, xtickn=notn, ytit='e residual', elems[*,1] - $
              sqrt( vv^2 + (1-vv^2)*epsilon^2 + $
                    2*vv*(1+vv)*epsilon*cos(xi) )

  ; Plot epicyclic elements
  plot_nosci, r0, /xs, /ys, xtickn=notn, ytit='r0'
  plot_nosci, epsilon, /xs, /ys, xtickn=notn, ytit='epsilon'
  plot_nosci, theta00*180/!dpi, /xs, /ys, ytit='theta00'
  plot_nosci, xi*180/!dpi, /xs, /ys, ytit='xi'

  !y.margin = oldym
  !y.omargin = oldyom

endif
if keyword_set(debug) then stop

out = [ [r0], [epsilon], [elems[*,2:3]], $
        [fix_angles(theta00-elems[*,3],/rad,/to360)], [xi] ]
if countskip gt 0 then out[where(skip),*] = 0
return, out

end
