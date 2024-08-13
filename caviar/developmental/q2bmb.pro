;this routine was written by B. Byington in 2010/2011. v is the image
;number. this sped up the process of looking at differences in star
;pointing and edge pointing in the Cassini division.

repoint_array[1,v]=x_move
repoint_array[2,v]=xmsig
repoint_array[3,v]=y_move
repoint_array[4,v]=ymsig
save, repoint_array, filename='repoint.sav'

end
