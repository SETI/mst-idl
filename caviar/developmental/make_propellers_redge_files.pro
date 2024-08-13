restore, 'prop_reproj.sav'
restore, 'fit_propellers4.sav'
redgefiles = where( _flag eq 'redge', count )
print, 'Out of '+strtrim(n_elements(prop_reproj),2)+', the following '+$
       strtrim(count,2)+' propellers are selected for prop_reproj_redge.sav:'
print, redgefiles
print, 'Proceed?'
stop

prop_reproj = prop_reproj[redgefiles]

save, prop_reproj, filename='prop_reproj_redge.sav'

end
