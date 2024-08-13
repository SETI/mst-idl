lzr,'diffring_exposures',/half
@plot_prepare                 
plot_color

allfilters=0
usenorm=1
bg=0
filter=1
.run run_diffring_exposures   

allfilters=1               
.run run_diffring_exposures

usenorm=0
bg=1
.run run_diffring_exposures

clzr
