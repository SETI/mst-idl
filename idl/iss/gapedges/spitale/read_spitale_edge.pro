openr, 1, 'N1493613276_1-edge.txt'
aa = ''
redge_spitale = [ 0.0d0, 0.0d0 ]
while not eof(1) do begin
  readf, 1, aa
  spaces = [ strsplit(aa,' '), strlen(aa)+1 ]
  redge_spitale = [ [redge_spitale], $
                    [ strmid( aa, spaces[1], spaces[2]-spaces[1] ), $
                      strmid( aa, spaces[0], spaces[1]-spaces[0] ) ] ]
endwhile
close, 1
redge_spitale = redge_spitale[*,1:n_elements(redge_spitale[0,*])-1]

save, redge_spitale, filename='N1493613276_spitale.edge'

end
