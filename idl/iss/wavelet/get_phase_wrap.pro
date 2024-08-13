function get_phase_wrap, phase, wrapsign=wrapsign, radians=radians

if keyword_set(radians) then qq=!pi else qq=180
nph = n_elements(phase)
dph = phase[1:nph-1] - phase[0:nph-2]
wrap = where( abs(dph) gt qq, nwrap ) + 1
if nwrap eq 0 then return, -1
wrapsign = -sign( dph[wrap-1] )
wrap = [ 0, wrap, nph ]
wrapsign = [ 0, wrapsign, 0 ]

return, wrap

end
