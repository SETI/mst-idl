;This program was written by Breanna Byington so that she didn't have
;to type as much. Q stands for quick. This can speed up making edge
;profiles in the Cassini division

restore, 'stretch.sav
restore, 'repoint.sav
device, retain=2
image_name=filenames[v]
nosmooth=0
nostars=1
newnacmat=2
hn=118260.
hm=118300.
rn=118600.
rm=118650.
jn=118940.
jm=118990.
kn=119380.
km=119430.
@caviar

end
