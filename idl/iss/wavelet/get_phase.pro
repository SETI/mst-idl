function get_phase, wave, wrap=wrap

phase = atan( imaginary(wave) / real_part(wave) ) * 180 / !pi
foo = where( real_part(wave) lt 0, count )
if count gt 0 then phase[foo] = phase[foo] + 180
foo = where( phase gt 180, count )
if count gt 0 then phase[foo] = phase[foo] - 360

wrap = get_phase_wrap(phase)

return, phase

end
