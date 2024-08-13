.run wavetest
psi0 = !dpi^(-0.25)
cdelta = 1
vfilt = dj * sqrt(dt) / (cdelta*psi0) * ( float(wave)  # (1./sqrt(scale)) )
vrejec = sst - vfilt

print, ''
print, 'mean(vfilt/recon_sst) = '+strtrim(mean(vfilt/recon_sst),2)
print, 'stddev(vfilt/recon_sst) = '+strtrim(stddev(vfilt/recon_sst),2)
print, 'That should be 0.776 and 0, both are reconstructions from the same wave.'
print, ''
print, 'mean(abs(sst/recon_sst)) = '+strtrim(mean(abs(sst/recon_sst)),2)
print, 'stddev(abs(sst/recon_sst)) = '+strtrim(stddev(abs(sst/recon_sst)),2)
print, 'That should be 1 and 0, but it isn''t.'
print, ''
print, 'mean(abs(sst/vfilt)) = '+strtrim(mean(abs(sst/vfilt)),2)
print, '1/mean(abs(sst/vfilt)) = '+strtrim(1/mean(abs(sst/vfilt)),2)
print, 'stddev(abs(sst/vfilt)) = '+strtrim(stddev(abs(sst/vfilt)),2)
print, 'That should be 0.776 and 0, but it isn''t.'
print, ''
plot, abs(sst/vfilt)
