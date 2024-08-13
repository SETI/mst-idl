

stop

if !d.name eq 'X' then window, 2
run_diffring_exposures

if !d.name eq 'X' then window, 3
run_diffring_exposures, usenorm=0, /dn

end
