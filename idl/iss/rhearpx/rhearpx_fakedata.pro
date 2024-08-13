_fac = [ 1, 2, 4, 8, 16, 32 ]
nfac = n_elements(_fac)
if not keyword_exists(kkk) then kkk = -1
kkk = kkk + 1
restore, 'stretch.sav'
restore, '../rhea_reproject_grp.sav'
jjj = 6
image_name = filenames[jjj]
@caviar

ring_rads = 764.0d0 * 2.5
startlon = 0
stoplon = 360
ring_npoints = 1440.0
get_ring,et,ring_rads,startlon,stoplon,polera,poledec,ring_npoints,ring1,605L,light_time=light_time
get_ring,et,ring_rads+30,startlon,stoplon,polera,poledec,ring_npoints,ring2,605L,light_time=light_time
image_coords,ring1,cmat,vobs_planet,cam_params,nl,rcrds1
image_coords,ring2,cmat,vobs_planet,cam_params,nl,rcrds2
;rawim[rcrds[*,1],rcrds[*,0]] = rawim[rcrds[*,1],rcrds[*,0]] + rms[jjj]*_fac[kkk]
fakering = bytarr(10240,10240)
fakering[polyfillv( [rcrds1[*,1],reverse(rcrds2[*,1])]*10, $
                    [rcrds1[*,0],reverse(rcrds2[*,0])]*10, 10240, 10240 )] = 1
fakering = rebin( float(fakering), 1024, 1024 )
rawim = rawim + fakering*rms[jjj]*_fac[kkk]

.run rhea_reproject
tvscl, imrz>(-rms[jjj])<rms[jjj]
sz = size(imrz)
if not keyword_set(__imrz_fake) then __imrz_fake = fltarr( sz[1], sz[2], nfac )
__imrz_fake[*,*,kkk] = imrz
print, strtrim(kkk,2)+' / '+strtrim(nfac,2)+' :  fac='+strtrim(_fac[kkk],2)

save, _fac, nfac, __imrz_fake, jjj, filename='rhearpx_fakedata.sav'
